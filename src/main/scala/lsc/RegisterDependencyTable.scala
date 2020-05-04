package lsc

import boom.exu.CommitSignals
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._
import chisel3.iotesters.PeekPokeTester
import freechips.rocketchip.util.{DescribedSRAM, PopCountAtLeast}

/**
  * Custom commit signals combining ROBs commit signals
  * with a tag that could arrive from somewhere else (like the FTQ)
  *
  * @param
  */

abstract class RegisterDependencyTable(implicit p: Parameters) extends BoomModule {
  val ibdaParams = boomParams.ibdaParams.get
  val io = IO(new RdtIO)
}


class RdtIO(implicit p: Parameters) extends BoomBundle {
  val update = Input(Vec(decodeWidth, new RdtUpdateSignals))
  val mark = Output(Vec(boomParams.ibdaParams.get.rdtIstMarkWidth, new IstMark))
}

class RdtUpdateSignals(implicit p: Parameters) extends BoomBundle {
  val valid = Bool()
  val tag = UInt(boomParams.ibdaParams.get.ibda_tag_sz.W)
  val uop = new MicroOp
}

class MultiWriteSram(size: Int, width: Int, reads: Int, writes: Int) extends Module {
  val io = IO(new Bundle() {
    val write = Input(Vec(writes, new Bundle {
      val addr = UInt(log2Up(size).W)
      val data = UInt(width.W)
      val en = Bool()
    }))
    val read = Vec(reads, new Bundle() {
      val addr = Input(UInt(log2Up(size).W))
      val data = Output(UInt(width.W))
    })
  })
  // this will be turned to bits because it needs multiple writes - sync becaue it is read in the same cycle as the srams
  val ways = SyncReadMem(size, UInt(log2Up(writes).W))

  val srams = io.write.map(_ => SyncReadMem(size, UInt(width.W)))
  val test = SyncReadMem(size, UInt(width.W))
  val test_valid = RegInit(VecInit(Seq.fill(size)(false.B)))
  for ((w, i) <- io.write zipWithIndex) {
    when(w.en) {
      srams(i).write(w.addr, w.data)
      // TODO: check colliding writes
      ways(w.addr) := i.U
      test(w.addr) := w.data
      test_valid(w.addr) := true.B
    }
  }

  io.read.foreach(r => {
    val way = WireInit(ways.read(r.addr))
    way.suggestName("way")
    dontTouch(way)
    val reads = VecInit(srams.map(s => s.read(r.addr)))
    reads.suggestName("reads")
    dontTouch(reads)
    val read_data = WireInit(reads(way))
    read_data.suggestName("read_data")
    dontTouch(read_data)
    r.data := read_data
    val read_valid = WireInit(test_valid(RegNext(r.addr)))
    read_valid.suggestName("read_valid")
    dontTouch(read_valid)
    val test_read = WireInit(test(r.addr))
    test_read.suggestName("test_read")
    dontTouch(test_read)
    assert(!read_valid || (test_read === read_data), "failure in MultiWriteSram")
  })
}

object MultiWriteSramTester extends App {
  val writes = 2
  val size = 16
  chisel3.iotesters.Driver.execute(
    Array(
      "--backend-name","verilator",
      "--generate-vcd-output","on",
    ),
    () => new MultiWriteSram(size, 32, reads = 1, writes = writes)
  ) {
    (c) =>
      new PeekPokeTester(c) {
        val r = scala.util.Random
        c.io.write.foreach(w => {
          poke(w.en, false.B)
        })
        // read empty
        for(i <- 0 until 16 by 1){
          poke(c.io.read(0).addr, i)
          step(1)
        }
        // write to different addr
        for(i <- 0 until 16 by 2){
          for(j <- 0 until writes){
            poke(c.io.write(j).addr, i+j)
            poke(c.io.write(j).data, i+j)
            poke(c.io.write(j).en, true)
          }
          //read at the same time
          poke(c.io.read(0).addr, i)
          step(1)
          c.io.write.foreach(w => {
            poke(w.en, false.B)
          })
          for(j <- 0 until writes){
            poke(c.io.read(0).addr, i+j)
            step(1)
            expect(c.io.read(0).data, i+j)
          }
        }
        // write to same addr
        for(i <- 0 until 16 by 2){
          for(j <- 0 until writes){
            poke(c.io.write(j).addr, i)
            poke(c.io.write(j).data, i+j)
            poke(c.io.write(j).en, true)
          }
          step(1)
          c.io.write.foreach(w => {
            poke(w.en, false.B)
          })
          poke(c.io.read(0).addr, i)
          step(1)
          // highest write port wins
          expect(c.io.read(0).data, i+writes-1)
        }
        step(1)
      }
  }
}

class RdtSyncMem(implicit p: Parameters) extends RegisterDependencyTable {

  val sram = Module(new MultiWriteSram(boomParams.numIntPhysRegisters, ibdaParams.ibda_tag_sz, ibdaParams.rdtIstMarkWidth, decodeWidth))
  val in_ist = RegInit(VecInit(Seq.fill(boomParams.numIntPhysRegisters)(false.B)))
  val commit_dst_valid = WireInit(VecInit(Seq.fill(decodeWidth)(false.B)))

  // Stage 1
  val rdt1_update = Wire(Vec(decodeWidth, new RdtUpdateSignals()))
  val rdt1_sram_read_addr = WireInit(VecInit(Seq.fill(ibdaParams.rdtIstMarkWidth)(0.U(log2Ceil(boomParams.numIntPhysRegisters).W))))

  // Stage 2
  val rdt2_mark_tag = Wire(Vec(ibdaParams.rdtIstMarkWidth, UInt(ibdaParams.ibda_tag_sz.W)))
  val rdt2_mark_valid = Wire(Vec(ibdaParams.rdtIstMarkWidth, Bool()))
  val rdt2_sram_tag = Wire(Vec(ibdaParams.rdtIstMarkWidth, UInt(ibdaParams.ibda_tag_sz.W)))
  val rdt2_sram_in_ist = Reg(Vec(ibdaParams.rdtIstMarkWidth, Bool()))
  val rdt2_bypass_tag = Reg(Vec(decodeWidth, UInt(ibdaParams.ibda_tag_sz.W)))
  val rdt2_bypass_in_ist = Reg(Vec(decodeWidth, Bool()))
  val rdt2_bypass_valid = Reg(Vec(decodeWidth, Bool()))
  val rdt2_bypass_select = Reg(Vec(ibdaParams.rdtIstMarkWidth, Bool()))


  // Mark port arbiting
  // To track which ports have already been used to mark and when we are full
  val mark_port_idx = Wire(Vec(decodeWidth * 2, UInt(ibdaParams.rdtIstMarkSz.W)))
  val mark_port_used = WireInit(VecInit(Seq.fill(decodeWidth * 2)(false.B)))
  val mark_port_full = Wire(Vec(decodeWidth * 2, Bool()))

  // Stage 1
  for (i <- 0 until decodeWidth) {
    // IO
    rdt1_update(i) := io.update(i)
  }


  for (i <- 0 until ibdaParams.rdtIstMarkWidth) {
    // SRAM read
    sram.io.read(i).addr := (rdt1_sram_read_addr(i))
    rdt2_sram_tag(i) := sram.io.read(i).data
    rdt2_sram_in_ist(i) := in_ist(rdt1_sram_read_addr(i))

    // IO
    io.mark(i).mark.valid := rdt2_mark_valid(i)
    io.mark(i).mark.bits := rdt2_mark_tag(i)

    // Default value for bypass
    rdt2_bypass_tag(i) := 0.U
    rdt2_bypass_valid(i) := false.B
    rdt2_bypass_in_ist(i) := true.B
    rdt2_bypass_select(i) := true.B

  }

  //Calculate the Read addresses
  for (i <- 0 until decodeWidth) {
    val uop = io.update(i).uop
    val valid = io.update(i).valid
    val tag = io.update(i).tag

    val is_b = boomParams.ibdaParams.get.is_ibda(uop)


    // First the logic for getting which mark_port to use and whether it is already full.
    //  The logic is based on a SW for-loop. mark_port_idx of current iteration depends on the last iteration
    //  mark_port_full of this iteration depends on whether last iteration were already full or if it used the port
    //  and we now are full. It is a little messy since we are looping through 2 ports per iteration (1 per source register)

    if (i == 0) { // Special case of first dispatched instr. At this point we dont depend on any previous iteration
      mark_port_idx(0) := 0.U
      mark_port_full(0) := false.B

    } else { // Normal case
      mark_port_idx(2 * i + 0) := Mux(mark_port_used(2 * i - 1), mark_port_idx(2 * i - 1) + 1.U, mark_port_idx(2 * i - 1))
      mark_port_full(2 * i + 0) := Mux(mark_port_used(2 * i - 1) &&
        mark_port_idx(2 * i) === ibdaParams.rdtIstMarkWidth.asUInt(), true.B, mark_port_full(2 * i - 1))
    }

    // Can we try to mark RS2?
    mark_port_idx(2 * i + 1) := Mux(mark_port_used(2 * i), mark_port_idx(2 * i) + 1.U, mark_port_idx(2 * i))
    mark_port_full(2 * i + 1) := Mux(mark_port_used(2 * i) &&
      mark_port_idx(2 * i + 1) === ibdaParams.rdtIstMarkWidth.asUInt(),
      true.B, mark_port_full(2 * i))


    sram.io.write(i).en := false.B
    sram.io.write(i).addr := DontCare
    sram.io.write(i).data := DontCare
    // record pc of last insn that writes to reg
    when(valid && uop.dst_rtype === RT_FIX && uop.pdst =/= 0.U) {
      sram.io.write(i).en := true.B
      sram.io.write(i).addr := uop.pdst
      sram.io.write(i).data := tag
      in_ist(uop.pdst) := is_b
      commit_dst_valid(i) := true.B
    }

    // Mark source register 1
    when(valid && is_b) {
      val port_idx = mark_port_idx(2 * i)
      val port_used = mark_port_used(2 * i)
      val port_full = mark_port_full(2 * i)
      when(uop.lrs1_rtype === RT_FIX && uop.prs1 =/= 0.U) {
        when(!port_full) {
          port_used := !in_ist(uop.prs1)
          rdt1_sram_read_addr(port_idx) := uop.prs1
          rdt2_bypass_select(port_idx) := false.B

          // bypass rdt for previous insns written in same cycle
          for (j <- 0 until i) {
            val uop_j = io.update(j).uop
            val uop_j_in_ist = boomParams.ibdaParams.get.is_ibda(uop_j)
            when(commit_dst_valid(j) && uop_j.pdst === uop.prs1) {
              port_used := !uop_j_in_ist
              rdt2_bypass_tag(port_idx) := io.update(j).tag
              rdt2_bypass_in_ist(port_idx) := uop_j_in_ist
              rdt2_bypass_select(port_idx) := true.B
            }
          }
          when(port_used) {
            in_ist(uop.prs1) := true.B
          }
        }
      }


      // Mark source register 2
      when(uop.lrs2_rtype === RT_FIX &&
        uop.prs2 =/= 0.U &&
        !(uop.uopc === uopSTA || uop.uopc === uopSTD)) { //Dont mark RS2 of stores, since its the data generation
        val port_idx = mark_port_idx(2 * i + 1)
        val port_used = mark_port_used(2 * i + 1)
        val port_full = mark_port_full(2 * i + 1)
        when(!port_full) {
          rdt1_sram_read_addr(port_idx) := uop.prs2
          port_used := !in_ist(uop.prs2)
          rdt2_bypass_select(port_idx) := false.B

          // bypass rdt for previous insns written in same cycle
          for (j <- 0 until i) {
            val uop_j = io.update(j).uop
            val uop_j_in_ist = boomParams.ibdaParams.get.is_ibda(uop_j)
            when(commit_dst_valid(j) && uop_j.pdst === uop.prs2) {
              port_used := !uop_j_in_ist
              rdt2_bypass_tag(port_idx) := io.update(j).tag
              rdt2_bypass_in_ist(port_idx) := uop_j_in_ist
              rdt2_bypass_select(port_idx) := true.B
            }
          }
          when(port_used) {
            in_ist(uop.prs2) := true.B
          }
        }
      }
    }
  }

  // Do output MUX
  for (i <- 0 until ibdaParams.rdtIstMarkWidth) {
    rdt2_mark_valid(i) := Mux(rdt2_bypass_select(i), !rdt2_bypass_in_ist(i), !rdt2_sram_in_ist(i))
    rdt2_mark_tag(i) := Mux(rdt2_bypass_select(i), rdt2_bypass_tag(i), rdt2_sram_tag(i))
  }

}


//class RdtOneBit(implicit p: Parameters) extends RegisterDependencyTable {
//  val rdt = Reg(Vec(boomParams.numIntPhysRegisters, UInt(ibdaParams.ibda_tag_sz.W)))
//  val in_ist = RegInit(VecInit(Seq.fill(boomParams.numIntPhysRegisters)(false.B)))
//  val commit_dst_valid = WireInit(VecInit(Seq.fill(decodeWidth)(false.B)))
//
//  io.mark := DontCare
//  io.mark.map(_.mark.valid := false.B)
//
//  // To track which ports have already been used to mark and when we are full
//  val mark_port_idx = Wire(Vec(decodeWidth * 2, UInt(log2Ceil(ibdaParams.rdtIstMarkWidth).W)))
//  val mark_port_used = WireInit(VecInit(Seq.fill(decodeWidth*2)(false.B)))
//  val mark_port_full = Wire(Vec(decodeWidth * 2, Bool()))
//
//  //TODO: Remove these DontTouches
//  dontTouch(mark_port_used)
//  dontTouch(mark_port_full)
//
//  for (i <- 0 until decodeWidth) {
//    val uop = io.update(i).uop
//    val valid = io.update(i).valid
//    val tag = io.update(i).tag
//
//    val is_b = uop.is_lsc_b || uop.uopc === uopLD || uop.uopc === uopSTA || uop.uopc === uopSTD
//
//    // First the logic for getting which mark_port to use and wether it is already full.
//    //  The logic is based on a SW for-loop. mark_port_idx of current iteration depends on the last iteration
//    //  mark_port_full of this iteration depends on wether last iteration were already full or if it used the port
//    //  and we now are full. It is a little messy since we are looping through 2 ports per iteration (1 per source register)
//
//    if (i == 0) { // Special case of first dispatched instr. At this point we dont depend on any previous iteration
//      mark_port_idx(0) := 0.U
//      mark_port_full(0) := false.B
//
//    } else { // Normal case
//      mark_port_idx(2*i + 0) := Mux(mark_port_used(2*i - 1), mark_port_idx(2*i - 1) + 1.U, mark_port_idx(2*i - 1))
//      mark_port_full(2*i + 0) := Mux(mark_port_used(2*i - 1) &&
//        mark_port_idx(2 * i) === ibdaParams.rdtIstMarkWidth.asUInt(), true.B, mark_port_full(2*i - 1))
//    }
//
//
//    // record pc of last insn that writes to reg
//    when(valid && uop.dst_rtype === RT_FIX && uop.pdst =/= 0.U) {
//      rdt(uop.pdst) := tag
//      in_ist(uop.pdst) := is_b
//      commit_dst_valid(i) := true.B
//    }
//    // Mark source register 1
//    when(valid && is_b) {
//      val port_idx = mark_port_idx(2*i)
//      val port_used = mark_port_used(2*i)
//      val port_full = mark_port_full(2*i)
//      when(uop.lrs1_rtype === RT_FIX && uop.prs1 =/= 0.U) {
//        when(!port_full) {
//          io.mark(port_idx).mark.valid := !in_ist(uop.prs1) // Only valid when RS1 is not already in IST
//          port_used := !in_ist(uop.prs1)
//          io.mark(port_idx).mark.bits := rdt(uop.prs1)
//          // bypass rdt for previous insns written in same cycle
//          for (j <- 0 until i) {
//            val uop_j = io.update(j).uop
//            val uop_j_in_ist = (uop_j.is_lsc_b || uop_j.uopc === uopLD || uop_j.uopc === uopSTA || uop_j.uopc === uopSTD)
//            when(commit_dst_valid(j) && uop_j.pdst === uop.prs1) {
//              io.mark(port_idx).mark.valid := !uop_j_in_ist
//              port_used := !uop_j_in_ist
//              io.mark(port_idx).mark.bits := io.update(j).tag
//            }
//          }
//          when (port_used) {
//            in_ist(uop.prs1) := true.B
//          }
//        }
//      }
//      // Can we try to mark RS2?
//      mark_port_idx(2*i + 1) := Mux(mark_port_used(2*i), mark_port_idx(2*i) + 1.U, mark_port_idx(2*i))
//      mark_port_full(2*i + 1) := Mux(mark_port_used(2*i) &&
//        mark_port_idx(2 * i + 1) === ibdaParams.rdtIstMarkWidth.asUInt(),
//        true.B, mark_port_full(2*i))
//
//      // Mark source register 2
//      when(uop.lrs2_rtype === RT_FIX &&
//        uop.prs2 =/= 0.U &&
//        !(uop.uopc === uopSTA || uop.uopc === uopSTD) ) {//Dont mark RS2 of stores, since its the data generation
//        val port_idx = mark_port_idx(2*i + 1)
//        val port_used = mark_port_used(2*i + 1)
//        val port_full = mark_port_full(2*i + 1)
//        when(!port_full) {
//          io.mark(port_idx).mark.valid := !in_ist(uop.prs2) // Only valid when RS1 is not already in IST
//          io.mark(port_idx).mark.bits := rdt(uop.prs2)
//          port_used := !in_ist(uop.prs2)
//          // bypass rdt for previous insns written in same cycle
//          for (j <- 0 until i) {
//            val uop_j = io.update(j).uop
//            val uop_j_in_ist = (uop_j.is_lsc_b || uop_j.uopc === uopLD || uop_j.uopc === uopSTA || uop_j.uopc === uopSTD)
//            when(commit_dst_valid(j) && uop_j.pdst === uop.prs2) {
//              io.mark(port_idx).mark.valid := !uop_j_in_ist
//              port_used := !uop_j_in_ist
//              io.mark(port_idx).mark.bits := io.update(j).tag
//            }
//          }
//          when (port_used) {
//            in_ist(uop.prs2) := true.B
//          }
//        }
//      }
//    }
//  }
//}
//
//
//
//class RdtBasic(implicit p: Parameters) extends RegisterDependencyTable {
//
//  val rdt = Reg(Vec(boomParams.numIntPhysRegisters, UInt(ibdaParams.ibda_tag_sz.W)))
//  val commit_dst_valid = WireInit(VecInit(Seq.fill(decodeWidth)(false.B)))
//
//  io.mark := DontCare
//  io.mark.map(_.mark.valid := false.B)
//
//  for(i <- 0 until decodeWidth){
//
//
//    val uop = io.update(i).uop
//    val valid = io.update(i).valid
//    val tag = io.update(i).tag
//
//    // record pc of last insn that committed to reg
//    when(valid && uop.dst_rtype === RT_FIX && uop.pdst =/= 0.U){
//      rdt(uop.pdst) := tag
//      commit_dst_valid(i) := true.B
//    }
//
//    val is_b = uop.is_lsc_b || uop.uopc === uopLD || uop.uopc === uopSTA || uop.uopc === uopSTD
//    // add pcs of dependent insns to ist
//    when(valid && is_b){
//      when(uop.lrs1_rtype === RT_FIX && uop.prs1 =/= 0.U){ //TODO: Remove use of logical register specifiers.
//        io.mark(2*i).mark.valid := true.B
//        io.mark(2*i).mark.bits := rdt(uop.prs1)
//        // bypass rdt for previous insns committed in same cycle
//        for (j <- 0 until i){
//          val uop_j = io.update(j).uop
//          when(commit_dst_valid(j) && uop_j.pdst === uop.prs1){
//            io.mark(2*i).mark.bits := io.update(j).tag
//          }
//        }
//      }
//      when(uop.lrs2_rtype === RT_FIX && uop.prs2 =/= 0.U){
//        io.mark(2*i+1).mark.valid := true.B
//        io.mark(2*i+1).mark.bits := rdt(uop.prs2)
//        // bypass rdt for previous insns committed in same cycle
//        for (j <- 0 until i){
//          val uop_j = io.update(j).uop
//          when(commit_dst_valid(j) && uop_j.pdst === uop.prs2){
//            io.mark(2*i+1).mark.bits := io.update(j).tag
//          }
//        }
//      }
//    }
//  }
//}