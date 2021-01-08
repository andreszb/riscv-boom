package exu.dispatch

import chisel3.iotesters.PeekPokeTester
import boom.common.MicroOp
import boom.exu.BrUpdateInfo
import boom.util.{GetNewBrMask, IsKilledByBranch, WrapAdd}
import chisel3._
import chisel3.util._
import treadle.executable.DataType
import freechips.rocketchip.config.Parameters

case class QueueParams(
                        val numEntries: Int,
                        val deqWidth: Int,
                        val enqWidth: Int,
                        val qName: String,
                        val stallOnUse: Boolean = false,
                        val headRegisters: Boolean = false,
                        val bufferRegisters: Boolean = false,
                        val shiftThrough: Boolean = false
                      )

class AbstractQueueIO[DataClass <: Data, UpdateClass <: Data](params: QueueParams, dt: DataClass, up: UpdateClass)
  extends Bundle {
  val enq = Vec(params.enqWidth, Flipped(DecoupledIO(dt.cloneType)))
  val heads = Vec(params.deqWidth, DecoupledIO(dt.cloneType))
  val update = Input(up.cloneType)
  val flush = Input(new Bool)
}

abstract class AbstractQueue[DataClass <: Data, UpdateClass <: Data]
(params: QueueParams, dt: DataClass, up: UpdateClass) extends Module with QueueUpdate[DataClass, UpdateClass] {
  val io = IO(new AbstractQueueIO[DataClass, UpdateClass](params, dt, up))
}

trait QueueUpdate[DataClass <: Data, UpdateClass <: Data] {
  def update(dt: DataClass, vl: Bool, up: UpdateClass): (DataClass, Bool)
  def merge(dt: DataClass, up: DataClass): DataClass
}

abstract class AbstractShiftQueue[DataClass <: Data, UpdateClass <: Data]
(params: QueueParams, dt: DataClass, up: UpdateClass) extends AbstractQueue[DataClass, UpdateClass](params, dt, up) {
  require(!params.headRegisters, "AbstractShiftQueue can't have head registers")
  require(!params.bufferRegisters, "AbstractShiftQueue can't have buffer registers")

  val data = Reg(Vec(params.numEntries, dt.cloneType))
  val valids = RegInit(VecInit(Seq.fill(params.numEntries)(false.B)))
  //update logic before flush
  val tmp_update_data = data.zip(valids).map { case (dt, vl) => update(dt, vl, io.update) }.unzip
  val updated_data = WireInit(VecInit(tmp_update_data._1))
  val updated_valids = WireInit(VecInit(tmp_update_data._2))

  val tmp_enq_data = io.enq.map { d => update(d.bits, d.valid, io.update) }.unzip
  val enq_data = WireInit(VecInit(tmp_enq_data._1))
  val enq_valids = WireInit(VecInit(tmp_enq_data._2))

  when(io.flush) {
    updated_valids.foreach(_ := false.B)
    enq_valids.foreach(_ := false.B)
  }

  val all_data = updated_data ++ enq_data
  val all_valids = updated_valids ++ enq_valids
  var consumed = Array.fill(params.numEntries + params.enqWidth)(false.B)

  // deq logic
  var prev_fire = WireInit(true.B)
  io.heads.zipWithIndex.map { case (h, i) =>
    h.bits := updated_data(i)
    if (params.stallOnUse) {
      // only set to valid if previous head fired
      h.valid := updated_valids(i) && prev_fire
      prev_fire = h.fire()
    } else {
      h.valid := updated_valids(i)
    }
    consumed(i) = h.fire()
  }

  // shift logic - select earliest valid not consumed entry
  for (i <- 0 until params.numEntries) {
    var entry_filled = false.B
    valids(i) := false.B
    for (j <- i until params.numEntries + params.enqWidth) {
      val fill = !consumed(j) && all_valids(j) && !entry_filled
      when(fill) {
        data(i) := all_data(j)
        valids(i) := true.B
      }
      consumed(j) = consumed(j) || fill
      entry_filled = fill || entry_filled
    }
  }
  val final_consumed = WireInit(VecInit(consumed))
  dontTouch(final_consumed)

  io.enq.zipWithIndex.map { case (e, i) =>
    if (params.shiftThrough) {
      e.ready := final_consumed(params.numEntries + i)
    } else {
      // break up combinatorial path - accept if tail has space
      e.ready := !valids(params.numEntries - 1 - i)
      assert(!(enq_valids(i) ^ final_consumed(params.numEntries + i)), s"queue ${params.qName}: enq_valids($i) <=> consumed")
      // set enq as invalid if we can't guarantee space for it
      when(!e.ready) {
        enq_valids(i) := false.B
      }
    }
  }
}

abstract class AbstractMultiSRAMQueue[DataClass <: Data, UpdateClass <: Valid[Data]]
(params: QueueParams, dt: DataClass, up: UpdateClass) extends AbstractQueue[DataClass, UpdateClass](params, dt, up) {
  require(params.stallOnUse, "AbstractMultiSRAMQueue needs to be stall on use")
  require(!params.headRegisters, "AbstractMultiSRAMQueue can't have head registers")
  require(!params.bufferRegisters, "AbstractMultiSRAMQueue can't have buffer registers")
  require(!params.shiftThrough, "AbstractMultiSRAMQueue can't be shift through")

  val cols = math.max(params.enqWidth, params.deqWidth)
  require(params.numEntries%cols == 0)
  val rows = params.numEntries/cols
  val rowBits = log2Ceil(rows)
  val colBits = log2Ceil(cols)
  // 2d structures
  // updatable part of data
  val data = Reg(Vec(cols, Vec(rows,dt.cloneType)))
  val valids = RegInit(VecInit(Seq.fill(cols)(VecInit(Seq.fill(rows)(false.B)))))
  // we need writes to happen before reads
  val srams = Seq.fill(cols)(SyncReadMem(rows, dt.cloneType, SyncReadMem.WriteFirst))
  val sram_read_addrs = Wire(Vec(cols, UInt(rowBits.W)))
  val sram_read_data = WireInit(VecInit(srams.zip(sram_read_addrs).map{case(r, a) => r.read(a)}))  // pick right valids for sram reads
  val sram_read_valids = WireInit(VecInit(Seq.fill(cols)(false.B)))
  val sram_write_data = Wire(Vec(cols, dt.cloneType))
  val sram_write_addrs = Wire(Vec(cols, UInt(rowBits.W)))
  val sram_write_valids = WireInit(VecInit(Seq.fill(cols)(false.B)))
  sram_write_data := DontCare
  sram_write_addrs := DontCare

  srams.zipWithIndex.foreach{ case (s, i) =>
    when(sram_write_valids(i)){
      s.write(sram_write_addrs(i), sram_write_data(i))
    }}

  def forward(row: UInt, col: UInt): (UInt, UInt) = {
    if(isPow2(rows) && isPow2(cols)){
      println("forward pow2")
      val tmp = WireInit(Cat(row, col) + 1.U)
      (tmp(rowBits+colBits-1, colBits), tmp(colBits-1, 0))
    } else {
      val row_wrap = row === (rows - 1).U
      val col_wrap = col === (cols - 1).U
      val new_col = Mux(col_wrap, 0.U, col + 1.U)
      val new_row = Mux(col_wrap, Mux(row_wrap, 0.U, row + 1.U), row)
      (new_row, new_col)
    }
  }

  val tmp_enq_data = io.enq.map { d => update(d.bits, d.valid, io.update) }.unzip
  val enq_data = WireInit(VecInit(tmp_enq_data._1))
  val enq_valids = WireInit(VecInit(tmp_enq_data._2))

  val merged_sram_read_data = WireInit(VecInit(sram_read_data.zipWithIndex.map{
    case(d, i) => merge(d, data(i)(RegNext(sram_read_addrs(i))))}))

  // head that was used to read the srams last cycle
  val head_row = RegInit(0.U(rowBits.W))
  val head_col = RegInit(0.U(colBits.W))
  // head that is used to read sram this cycle
  val next_head_row = WireInit(head_row)
  val next_head_col = WireInit(head_col)
  head_row := next_head_row
  head_col := next_head_col
  // TODO: maybe optimize
  sram_read_addrs.zipWithIndex.foreach{
    case(a, i) => a := next_head_row + (i.U<next_head_col)
  }
  val tail_row = RegInit(0.U(rowBits.W))
  val tail_col = RegInit(0.U(colBits.W))
  val next_tail_row = WireInit(tail_row)
  val next_tail_col = WireInit(tail_col)
  tail_row := next_tail_row
  tail_col := next_tail_col
  sram_write_addrs.zipWithIndex.foreach{
    case(a, i) => a := tail_row + (i.U<tail_col)
  }

  // update logic before flush
  val updated_data = WireInit(data)
  val updated_valids = WireInit(valids)
  val updated_tail_col = WireInit(tail_col)
  val updated_tail_row = WireInit(tail_row)
  // structure is cyclic -> last
  var previous_valid = updated_valids(cols-1)(rows-1)
  for(r <- 0 until rows) {
    for(c <- 0 until cols){
      val v = valids(c)(r)
      val (ud, uv) = update(data(c)(r), v, io.update)
      updated_data(c)(r) := ud
      updated_valids(c)(r) := uv
      // transition point
      when(previous_valid && !uv){
        updated_tail_col := c.U
        updated_tail_row := r.U
        when(v && !uv){
          printf(s"transition at $r $c\n")
        }
      }
      previous_valid = uv
      dontTouch(previous_valid.suggestName(s"pv_${r}_${c}"))
    }
  }
  data := updated_data
  valids := updated_valids

  // pick right valids for sram reads
  updated_valids.zipWithIndex.map{ case (vlds, i) =>
    val r = head_row + (i.U<head_col)
    sram_read_valids(i) := vlds(r)
  }

  // deq logic
  var prev_fire = WireInit(true.B)
  var deq_row = head_row
  var deq_col = head_col
  io.heads.zipWithIndex.map { case (h, i) =>
    // we want the heads to be in order so we have to offset them
    val c = WrapAdd(head_col, i.U, cols)
    h.bits := sram_read_data(c)
    h.valid := sram_read_valids(c) && prev_fire
    prev_fire = h.fire()
    val (dr,dc) = forward(deq_row, deq_col)
    // move the head to the last fired one
    when(h.fire()){
      val r = head_row + (c<head_col)
      valids(c)(r) := false.B
      next_head_row := dr
      next_head_col := dc
    }
    deq_row = dr
    deq_col = dc
  }

  io.enq.foreach(_.ready := false.B)
  var enq_row = tail_row
  var enq_col = tail_col
  val enq_consumed = Array.fill(params.enqWidth)(false.B)
  for(i<- 0 until params.enqWidth){
    var slot_filled = false.B
    val has_space = !valids(enq_col)(enq_row)
    // we don't want a combinatorial path between valid and ready of enq
    io.enq(i).ready := has_space
    io.enq.zipWithIndex.foreach{ case (e, i) =>
      val fill = has_space && !slot_filled && !enq_consumed(i) && e.fire()
      when(fill){
        sram_write_valids(enq_col) := true.B
        sram_write_data(enq_col) := e.bits
        assert(enq_row === sram_write_addrs(enq_col))
        valids(enq_col)(enq_row) := true.B
        data(enq_col)(enq_row) := e.bits
      }
      enq_consumed(i) = enq_consumed(i) || fill
      slot_filled = slot_filled || fill
    }
    val (er, ec) = forward(enq_row, enq_col)
    when(slot_filled){
      next_tail_row := er
      next_tail_col := ec
    }
    enq_row = er
    enq_col = ec
  }

  // change tail on update
  when(io.update.valid){
    tail_row := updated_tail_row
    tail_col := updated_tail_col
    // either tail not changed or nothing enqueued
    assert((tail_row === updated_tail_row && tail_col === updated_tail_col) || !io.enq.map(_.fire()).reduce(_ || _))
    // flush to empty
    when(!updated_valids.flatten.reduce(_||_)){
      tail_row := head_row
      tail_col := head_col
    }
  }


  // reset everything on flush
  when(io.flush) {
// TODO: figure out if this is necessary:
//    updated_valids.foreach(r => r.foreach(_ := false.B))
//    enq_valids.foreach(_ := false.B)
    head_row := 0.U
    head_col := 0.U
    tail_row := 0.U
    tail_col := 0.U
    io.heads.foreach(_.valid := false.B)
    io.enq.foreach(_.ready := false.B)
  }

  when(!valids.flatten.reduce(_||_)){
    assert(head_col === tail_col && head_row === tail_row)
  }

  //debug
  dontTouch(valids)
  dontTouch(sram_write_valids)
  dontTouch(sram_write_data)
  dontTouch(sram_read_addrs)
  dontTouch(updated_valids)
  dontTouch(next_head_col)
  dontTouch(next_tail_row)
  dontTouch(next_tail_col)
}

trait UopQueueUpdate extends QueueUpdate[MicroOp, BrUpdateInfo] {
  override def update(dt: MicroOp, vl: Bool, up: BrUpdateInfo): (MicroOp, Bool) = {
    val br_mask = dt.br_mask
    val uop = WireInit(dt)
    uop.br_mask := GetNewBrMask(up, br_mask)
    (uop, vl && !IsKilledByBranch(up, br_mask))
  }

  override def merge(dt: MicroOp, up: MicroOp): MicroOp = {
    val uop = WireInit(dt)
    uop.br_mask := up.br_mask
    uop
  }
}

class UopShiftQueue(params: QueueParams)(implicit p: Parameters)
  extends AbstractShiftQueue[MicroOp, BrUpdateInfo](params, new MicroOp(), new BrUpdateInfo()) with UopQueueUpdate

trait TestQueueUpdate extends QueueUpdate[UInt, Valid[UInt]] {
  override def update(dt: UInt, vl: Bool, up: Valid[UInt]): (UInt, Bool) = {
    (dt, vl && !(up.valid && dt >= up.bits))
  }

  override def merge(dt: UInt, up: UInt): UInt = dt
}

class TestShiftQueue(params: QueueParams)
  extends AbstractShiftQueue[UInt, Valid[UInt]](params, UInt(32.W), Valid(UInt(32.W))) with TestQueueUpdate

class TestMultiSRAMQueue(params: QueueParams)
  extends AbstractMultiSRAMQueue[UInt, Valid[UInt]](params, UInt(32.W), Valid(UInt(32.W))) with TestQueueUpdate


object QueueTester extends App {
  val entries = 8
  val enq_width = 2
  val deq_width = 2
  chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on", "--backend-name", "verilator", "--is-verbose"), () =>
    new TestMultiSRAMQueue(QueueParams(entries, enq_width, deq_width, "test", stallOnUse = true))) {
    (c) =>
      new PeekPokeTester[TestMultiSRAMQueue](c) {
        private def fill() = {
          poke(c.io.enq(0).valid, true.B)
          for (i <- 0 until entries) {
            poke(c.io.enq(0).bits, i)
            expect(c.io.enq(0).ready, true.B)
            step(1)
            expect(c.io.heads(0).valid, true.B)
          }
          expect(c.io.enq(0).ready, false.B)
          poke(c.io.enq(0).valid, false.B)
        }

        private def flush(max: Int) = {
          poke(c.io.update.valid, true.B)
          poke(c.io.update.bits, max)
          step(1)
          poke(c.io.update.valid, false.B)
          poke(c.io.heads(0).ready, true.B)
        }

        // enq not valid
        for (i <- 0 until enq_width) {
          poke(c.io.enq(i).valid, false.B)
        }
        // deq not ready
        for (i <- 0 until deq_width) {
          poke(c.io.heads(i).ready, false.B)
        }
        // no branch or flush
        poke(c.io.update.valid, false.B)
        poke(c.io.flush, false.B)
        // wait for 10 cycles
        for (i <- 0 until 10) {
          step(1)
        }
        fill()
        // flush out half of queue
        flush(entries / 2)

        // check if flush worked - queue should have capacity again
        expect(c.io.enq(0).ready, true.B)
        // read out rest of queue
        for (i <- 0 until entries/2) {
          expect(c.io.heads(0).bits, i)
          expect(c.io.heads(0).valid, true.B)
          step(1)
        }
        // queue should be empty now
        expect(c.io.heads(0).valid, false.B)
        poke(c.io.heads(0).ready, false.B)
        step(1)
        fill()
        flush(0)
        step(1)
      }
  }
}