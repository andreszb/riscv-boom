package boom.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common.{IQT_MFP, MicroOp, O3PIPEVIEW_PRINTF, O3_START_CYCLE, uopLD, _}
import boom.util._
import chisel3.internal.naming.chiselName
import freechips.rocketchip.util.DescribedSRAM
import lsc.{MultiWriteSram, MultiWriteSramBase, MultiWriteSramTester}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class DispatchQueueParams(
                                val numEntries: Int,
                                val deqWidth: Int,
                                val enqWidth: Int,
                                val qName: String,
                                val stallOnUse: Boolean = false,
                                val headRegisters: Boolean = true,
                              )

class DispatchQueueIO(
                       val deqWidth: Int = 1,
                       val enqWidth: Int = 2)
                     (implicit p: Parameters) extends BoomBundle {
  val enq_uops = Vec(enqWidth, Flipped(DecoupledIO(new MicroOp)))
  val heads = Vec(deqWidth, DecoupledIO(new MicroOp()))
  val brinfo = Input(new BrResolutionInfo())
  val flush = Input(new Bool)
  val tsc_reg = Input(UInt(width=xLen.W)) // needed for pipeview
}



abstract class DispatchQueue( val numEntries: Int = 8,
                              val deqWidth: Int = 1,
                              val enqWidth: Int = 2,
                              val qName: String,
                              val stallOnUse: Boolean = false
                            )(implicit p: Parameters) extends BoomModule
{
  val io = IO(new DispatchQueueIO(deqWidth, enqWidth))
  val qAddrSz: Int = if(numEntries==1) 1 else log2Ceil(numEntries)
  val fifoWidth = if (enqWidth>deqWidth) enqWidth else deqWidth
  val qWidthSz: Int = if (fifoWidth == 1) {
    1
  } else {
    log2Ceil(fifoWidth)
  }

  // Maps i to idx of queue. Used with for-loops starting at head or tail
  def wrapIndex(i: UInt): UInt = {
    val out = Wire(UInt(qAddrSz.W))
    when(i <= numEntries.U) {
      out := i
    }.otherwise {
      out := i - numEntries.U
    }
    out
  }

  if(stallOnUse){
    val valid = RegInit(false.B)
    val prev_seq = RegInit(0.U(xLen.W))
    val max_seq = Wire(Vec(deqWidth+1, UInt(xLen.W)))
    max_seq(0) := prev_seq
    for(i <- 0 until deqWidth){
      val seq = io.heads(i).bits.debug_events.fetch_seq
      max_seq(i+1) := max_seq(i)
      when(io.heads(i).fire()){
        valid := true.B
        assert(!valid || seq > prev_seq, f"Stall on use queue $qName didn't issue in order!")
        when(max_seq(i) < seq){
          max_seq(i+1) := seq
        }
      }
    }
    prev_seq := max_seq(deqWidth)
  }
}

class UopSram(numEntries: Int, numWrites: Int, numReads: Int, registerBased: Boolean = false)(implicit p: Parameters) extends BoomModule{
  val io = IO(new Bundle{
    val writes = Input(Vec(numWrites, Valid(new Bundle{
      val addr = UInt(log2Ceil(numEntries).W)
      val data = new MicroOp()
    })))
    val read_req = Input(Vec(numReads, Valid(new Bundle{
      val addr = UInt(log2Ceil(numEntries).W)
    })))
    val read_resp = Output(Vec(numReads, Valid(new Bundle{
      val data = new MicroOp()
    })))
  })

  def bundle_to_hashmap(d: Data, name: String = "uop_sram", hashMap: mutable.HashMap[String, Data] = null): mutable.HashMap[String, Data] = {
    val hm = if(hashMap == null){
      new mutable.HashMap[String, Data]();
    } else {
      hashMap
    }
    d match {
      case vec: Vec[_] =>
        for ((c, i) <- vec.zipWithIndex) {
          bundle_to_hashmap(c, s"${name}_${i}", hm)
        }
      case bundle: Bundle =>
        bundle.elements.toList.foreach {
          case (st, d) => bundle_to_hashmap(d, s"${name}_${st}", hm)
        }
      case element: Element => {
        hm.put(name, d)
      }
      case _ => throw new scala.Exception(s"unknown type: $name")
    }
    hm
  }

  val default_hm = bundle_to_hashmap(new MicroOp())

  val whitelist = Seq[String](
    "uop_sram_uopc",
    "uop_sram_is_rvc",
    "uop_sram_iq_type",
    "uop_sram_fu_code",
    "uop_sram_is_br_or_jmp",
    "uop_sram_is_jump",
    "uop_sram_is_jal",
    "uop_sram_is_ret",
    "uop_sram_is_call",
    // not needed as it is stored seperately
//    "uop_sram_br_mask",
    "uop_sram_br_tag",
    "uop_sram_br_prediction_btb_blame",
    "uop_sram_br_prediction_btb_hit",
    "uop_sram_br_prediction_btb_taken",
    "uop_sram_br_prediction_bpd_blame",
    "uop_sram_br_prediction_bpd_taken",
    "uop_sram_ftq_idx",
    "uop_sram_cfi_idx",
    "uop_sram_edge_inst",
    "uop_sram_pc_lob",
    "uop_sram_imm_packed",
    "uop_sram_rob_idx",
    "uop_sram_ldq_idx",
    "uop_sram_stq_idx",
    "uop_sram_pdst",
    "uop_sram_prs1",
    "uop_sram_prs2",
    "uop_sram_prs3",
    "uop_sram_exception",
    "uop_sram_bypassable",
    "uop_sram_mem_cmd",
    "uop_sram_mem_size",
    "uop_sram_mem_signed",
    "uop_sram_is_fence",
    "uop_sram_is_fencei",
    "uop_sram_is_amo",
    "uop_sram_uses_ldq",
    "uop_sram_uses_stq",
    "uop_sram_ldst_val",
    "uop_sram_dst_rtype",
    "uop_sram_lrs1_rtype",
    "uop_sram_lrs2_rtype",
    "uop_sram_frs3_en",
    "uop_sram_fp_val",
//    "uop_sram_perf_dnb_dlq",
//    "uop_sram_perf_dnb_crq",
//    "uop_sram_perf_dnb_iq",
    //optional
    "uop_sram_debug_events_fetch_seq",
  )
  val mv_type = MixedVec(whitelist.map(n => default_hm(n)))

  if(registerBased){
    val reg = Reg(Vec(numEntries, new MicroOp()))
    for(wp <- io.writes){
      when(wp.valid){
        reg(wp.bits.addr) := wp.bits.data
      }
    }
    for((req, resp) <- io.read_req zip io.read_resp){
      resp.valid := RegNext(req.valid)
      val prev_addr = RegNext(req.bits.addr)
      when(resp.valid){
        resp.bits.data := reg(prev_addr)
      }.otherwise(
        resp.bits := DontCare
      )
    }
  } else if(numWrites==1){
    //  print(default_hm.keySet.reduce(_ + ",\n"+_))
    //  println()
    //  println()
    //  print(whitelist.toSet.diff(default_hm.keySet).reduce(_ + ",\n"+_))
    val uop_sram_compact = SyncReadMem(numEntries, mv_type)
    for(wp <- io.writes){
      val write_hm = bundle_to_hashmap(wp.bits.data)
      val mv = WireInit(MixedVecInit(whitelist.map(n => write_hm(n))))
      when(wp.valid){
        uop_sram_compact(wp.bits.addr) := mv
      }
    }
    val bypasses = RegNext(io.writes)
    for((req, resp) <- io.read_req zip io.read_resp){
      val read_hm = bundle_to_hashmap(resp.bits.data)
      resp.bits.data := DontCare
      val mv = uop_sram_compact.read(req.bits.addr, req.valid)
      for ((n, d) <- (whitelist zip mv)) {
        read_hm(n) := d
      }
      resp.valid := RegNext(req.valid)
      val bypass_addr = RegNext(req.bits.addr)
      for(bp <- bypasses){
        when(bp.valid && bypass_addr === bp.bits.addr){
          resp.bits.data := bp.bits.data
        }
      }
    }
  }else {
    //  print(default_hm.keySet.reduce(_ + ",\n"+_))
    //  println()
    //  println()
    //  print(whitelist.toSet.diff(default_hm.keySet).reduce(_ + ",\n"+_))
    val uop_sram_compact = Module(new MultiWriteSramBase(
      numEntries, UInt(), numReads, numWrites
    ))
    for((wp, i) <- io.writes.zipWithIndex){
      val write_hm = bundle_to_hashmap(wp.bits.data)
      val mv = WireInit(MixedVecInit(whitelist.map(n => write_hm(n))))
      uop_sram_compact.io.write(i).addr := wp.bits.addr
      uop_sram_compact.io.write(i).data := mv.asUInt()
      uop_sram_compact.io.write(i).en := wp.valid
    }
    val bypasses = RegNext(io.writes)
    for(((req, resp), i) <- io.read_req zip io.read_resp zipWithIndex){
      val read_hm = bundle_to_hashmap(resp.bits.data)
      resp.bits.data := DontCare
      val mv = uop_sram_compact.io.read(i).data.asTypeOf(mv_type)
      uop_sram_compact.io.read(i).addr := req.bits.addr
      for ((n, d) <- (whitelist zip mv)) {
        read_hm(n) := d
      }
      resp.valid := RegNext(req.valid)
      val bypass_addr = RegNext(req.bits.addr)
      for(bp <- bypasses){
        when(bp.valid && bypass_addr === bp.bits.addr){
          resp.bits.data := bp.bits.data
        }
      }
    }
  }

//  val uop_sram = SyncReadMem(numEntries, new MicroOp())
//  for(wp <- io.writes){
//    when(wp.valid){
//      uop_sram(wp.bits.addr) := wp.bits.data
//    }
//  }
//  val bypasses = RegNext(io.writes)
//  for((req, resp) <- io.read_req zip io.read_resp){
//    resp.bits.data := uop_sram.read(req.bits.addr, req.valid)
//    resp.valid := RegNext(req.valid)
//    val bypass_addr = RegNext(req.bits.addr)
//    for(bp <- bypasses){
//      when(bp.valid && bypass_addr === bp.bits.addr){
//        resp.bits.data := bp.bits.data
//      }
//    }
//  }

}


class NaiveDispatchQueueCompactingShifting(params: DispatchQueueParams,
                                         )(implicit p: Parameters) extends DispatchQueue(params.numEntries, params.deqWidth, params.enqWidth, params.qName, params.stallOnUse) {
  val reg_uops = RegInit(VecInit(Seq.fill(numEntries)(NullMicroOp())))
  val reg_valids = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val enq_uops = io.enq_uops.map(_.bits)
  // use same logic as issue unit
  val enq_valids = io.enq_uops.map(b =>
    b.valid &&
    !b.bits.exception &&
    !b.bits.is_fence &&
    !b.bits.is_fencei
  )
  // current uops
  val uops = reg_uops ++ enq_uops
  val valids = reg_valids ++ enq_valids

  // modified uops
  val mod_uops = WireInit(VecInit(uops))
  val mod_valids = WireInit(VecInit(valids))
  // after deq
  val next_valids = WireInit(mod_valids)

  // branch update
  when(io.brinfo.valid) {
    for((uop, i) <- uops.zipWithIndex){
      val br_mask = uop.br_mask
      val entry_match = valids(i) && maskMatch(io.brinfo.mask, br_mask)

      when (entry_match && io.brinfo.mispredict) { // Mispredict
        mod_valids(i) := false.B
      }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
        mod_uops(i).br_mask := (br_mask & ~io.brinfo.mask)
      }
    }
  }

  // flush update
  when(io.flush){
    mod_valids.foreach(_ := false.B)
  }

  var prev_fire = WireInit(true.B)
  for(i <- 0 until deqWidth){
    io.heads(i).bits := mod_uops(i)
    if(stallOnUse){
      io.heads(i).valid := mod_valids(i) && prev_fire
      prev_fire = io.heads(i).fire()
    } else{
      io.heads(i).valid := mod_valids(i)
    }
    when(io.heads(i).fire()){
      next_valids(i) := false.B
    }
  }

  // update uop reg
  // has this uop been placed?
  var consumed = Array.fill(uops.length)(false.B)
  for(i <- 0 until numEntries){
    // not valid if nothing is filled in
    reg_valids(i) := false.B
    // has this slot been filled?
    var entry_filled = false.B
    // this slot until deqWidth slots later
    val potential_slots_indices = i until scala.math.min(numEntries, i+deqWidth+1)
    // all enqs
    val enq_indices = numEntries until numEntries + enqWidth
    require(uops.length == (numEntries + enqWidth))
    for(j <- potential_slots_indices++enq_indices){
      val fill = !consumed(j) && next_valids(j) && !entry_filled
      when(fill){
        reg_uops(i) := mod_uops(j)
        reg_valids(i) := true.B
      }
      consumed(j) = consumed(j) || fill
      entry_filled = fill || entry_filled
    }
  }

  for(i <- 1 until numEntries){
    assert(!(valids(i) && !valids(i-1)), "valids may not have holes!")
  }

  val consumed_debug = WireInit(VecInit(consumed))
  dontTouch(consumed_debug)

  for(i <- 1 until numEntries){
    assert(!(consumed_debug(i)^next_valids(i)), "consumed <=> next_valid for slots")
  }

  // can not use consumed because otherwise a combinational loop is created
  for (i <- 0 until enqWidth) {
    // set later enqs high if earlier ones are not used
    val free_slots = (next_valids.take(numEntries)++enq_valids.take(i)).map(!_)
    val num_free_slots = PopCount(free_slots)
    io.enq_uops(i).ready := num_free_slots > i.U
    // don't use fire because we use the other valid
    assert(!((io.enq_uops(i).ready && enq_valids(i))^consumed_debug(numEntries+i)), f"enq($i).fire() <=> consumed! ")
  }

  if (O3PIPEVIEW_PRINTF) {
    when(io.tsc_reg>=O3_START_CYCLE.U) {
      for (i <- 0 until enqWidth) {
        when(io.enq_uops(i).valid) {
          printf("%d; O3PipeView:" + qName + ": %d\n",
            io.enq_uops(i).bits.debug_events.fetch_seq,
            io.tsc_reg)
        }
      }
    }
  }
}

class LayeredDispatchQueueCompactingShifting(params: DispatchQueueParams,
                                         )(implicit p: Parameters) extends DispatchQueue(params.numEntries, params.deqWidth, params.enqWidth, params.qName, params.stallOnUse) {
  val internal_queue = Module(new InternalSingleSramDispatchQueue(params.copy(stallOnUse = true, qName = qName+"_inner")))
  internal_queue.io.flush := io.flush
  internal_queue.io.brinfo := io.brinfo
  // no tsc for internal queue
  internal_queue.io.tsc_reg := 0.U

  val numHeads = if(params.headRegisters) deqWidth else 0
  require(params.headRegisters || params.stallOnUse, "not using stall on use requires head registers")

  val reg_head_uops = if(params.headRegisters) RegInit(VecInit(Seq.fill(numHeads)(NullMicroOp()))) else Seq()
  val reg_head_valids = if(params.headRegisters) RegInit(VecInit(Seq.fill(numHeads)(false.B))) else Seq()
  val enq_uops = io.enq_uops.map(_.bits)
  // use same logic as issue unit
  val enq_valids = io.enq_uops.map(b =>
    b.valid &&
    !b.bits.exception &&
    !b.bits.is_fence &&
    !b.bits.is_fencei
  )

  val internal_uops = internal_queue.io.heads.map(_.bits)
  val internal_valids = internal_queue.io.heads.map(_.valid)
  //TODO: prevent double-processing of internal uops

  // current uops
  val uops = reg_head_uops ++ internal_uops ++ enq_uops
  val valids = reg_head_valids ++ internal_valids ++ enq_valids

  // modified uops
  val mod_uops = WireInit(VecInit(uops))
  val mod_valids = WireInit(VecInit(valids))
  // after deq
  val next_valids = WireInit(mod_valids)

  // branch update
  when(io.brinfo.valid) {
    for((uop, i) <- uops.zipWithIndex){
      val br_mask = uop.br_mask
      val entry_match = valids(i) && maskMatch(io.brinfo.mask, br_mask)

      when (entry_match && io.brinfo.mispredict) { // Mispredict
        mod_valids(i) := false.B
      }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
        mod_uops(i).br_mask := (br_mask & ~io.brinfo.mask)
      }
    }
  }

  // flush update
  when(io.flush){
    mod_valids.foreach(_ := false.B)
  }


  // has this uop been placed/dequeued?
  var consumed = Array.fill(uops.length)(false.B)

  var prev_fire = WireInit(true.B)
  for(i <- 0 until deqWidth){
    // if there are head registers they will be used - otherwise the heads of the internal queue are used
    io.heads(i).bits := mod_uops(i)
    if(stallOnUse){
      io.heads(i).valid := mod_valids(i) && prev_fire
      prev_fire = io.heads(i).fire()
    } else{
      io.heads(i).valid := mod_valids(i)
    }
    consumed(i) = io.heads(i).fire()
    when(io.heads(i).fire()){
      next_valids(i) := false.B
    }
  }

  for(i <- 1 until numHeads){
    assert(!(valids(i) && !valids(i-1)), "valids may not have holes!")
  }

  // update uop reg
  for(i <- 0 until numHeads){
    // not valid if nothing is filled in
    reg_head_valids(i) := false.B
    // has this slot been filled?
    var entry_filled = false.B
    // all starting from current are potential candidates for head
    val potential_indices = i until numHeads + deqWidth + enqWidth
    for(j <- potential_indices){
      val fill = !consumed(j) && next_valids(j) && !entry_filled
      when(fill){
        reg_head_uops(i) := mod_uops(j)
        reg_head_valids(i) := true.B
      }
      consumed(j) = consumed(j) || fill
      entry_filled = fill || entry_filled
    }
  }

  // enq to internal
  for(i <- 0 until enqWidth){
    internal_queue.io.enq_uops(i).valid := false.B
    internal_queue.io.enq_uops(i).bits := DontCare
    // has this slot been filled?
    var entry_filled = !internal_queue.io.enq_uops(i).ready
    val enq_indices = numHeads+deqWidth until numHeads+deqWidth + enqWidth
    for(j <- enq_indices){
      val fill = !consumed(j) && next_valids(j) && !entry_filled
      when(fill){
        internal_queue.io.enq_uops(i).valid := true.B
        internal_queue.io.enq_uops(i).bits := mod_uops(j)
      }
      consumed(j) = consumed(j) || fill
      entry_filled = fill || entry_filled
    }
  }

  val consumed_debug = WireInit(VecInit(consumed))
  dontTouch(consumed_debug)

  // deq from internal
  for(i <- 0 until deqWidth) {
    internal_queue.io.heads(i).ready := consumed_debug(numHeads+i)
  }

  for(i <- 1 until numHeads){
    assert(!(consumed_debug(i)^next_valids(i)^io.heads(i).fire()), "consumed <=> next_valid for slots")
  }

  // can not use consumed because otherwise a combinational loop is created
  for (i <- 0 until enqWidth) {
    // set later enqs high if earlier ones are not used
    io.enq_uops(i).ready := internal_queue.io.enq_uops(i).ready
    // don't use fire because we use the other valid
    assert(!((io.enq_uops(i).ready && enq_valids(i))^consumed_debug(numHeads+deqWidth+i)), f"enq($i).fire() <=> consumed! ")
  }

  if (O3PIPEVIEW_PRINTF) {
    when(io.tsc_reg>=O3_START_CYCLE.U) {
      for (i <- 0 until enqWidth) {
        when(io.enq_uops(i).valid) {
          printf("%d; O3PipeView:" + qName + ": %d\n",
            io.enq_uops(i).bits.debug_events.fetch_seq,
            io.tsc_reg)
        }
      }
    }
  }
}


class InternalSingleSramDispatchQueue(params: DispatchQueueParams,
                                         )(implicit p: Parameters) extends DispatchQueue(params.numEntries, params.deqWidth, params.enqWidth, params.qName, params.stallOnUse)
{
  require(stallOnUse, "InternalSramDispatchQueue relies on stall on use!")
  val sram_fifo = Module(new UopSram(numEntries, enqWidth, deqWidth))

  // Branch mask and valid bits are still stored in Regs
  val totalEntires = numEntries*fifoWidth

  val br_masks = Reg(Vec(numEntries, UInt(maxBrCount.W) ))
  val valids = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val head = RegInit(0.U(qAddrSz.W))
  val tail = RegInit(0.U(qAddrSz.W))

  val mod_valids = WireInit(valids)
  val mod_br_masks = WireInit(br_masks)

  // branch update
  when(io.brinfo.valid) {
    for (r <- 0 until numEntries) {
      val br_mask = br_masks(r)
      val entry_match = valids(r) && maskMatch(io.brinfo.mask, br_mask)
      when (entry_match && io.brinfo.mispredict) { // Mispredict
        mod_valids(r) := false.B
      }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
        mod_br_masks(r) := (br_mask & ~io.brinfo.mask)
      }
    }
  }
  // up here so writes can go directly to br_masks below
  br_masks := mod_br_masks

  // update tail - branch
  val mod_tail = WireInit(tail)
  // go through from the back - relies on there being one continuous section of valids
  var previous = 0
  var transitions_mod_valids: List[Bool] = Nil
  // use Mux1H to prevent expensive long priority mux
  val mod_tail_mux_selector = new ListBuffer[Bool];
  val mod_tail_mux_val = new ListBuffer[UInt];
  for (r <- (0 until numEntries).reverse) {
    mod_tail_mux_selector += mod_valids(r) && !mod_valids(previous)
    mod_tail_mux_val += previous.U

    transitions_mod_valids = (mod_valids(r) && !mod_valids(previous)) :: transitions_mod_valids
    previous = r
  }
  assert(PopCount(transitions_mod_valids) <= 1.U, f"holes in mod_valids of $qName!")
  // noting valid - set to head
  when(!mod_valids.reduce(_||_)){
    mod_tail := head
  }.otherwise{
    mod_tail := Mux1H(mod_tail_mux_selector, mod_tail_mux_val)
  }

  val next_valids = WireInit(mod_valids)
  // update tail - enq
  val next_tail = WireInit(mod_tail)
  var enq_tail = mod_tail
  var previous_valid = true.B
  val sram_write_valids = Wire(Vec(enqWidth, Bool()))
  val sram_write_addrs = Wire(Vec(enqWidth, UInt()))
  val sram_write_data = Wire(Vec(enqWidth, new MicroOp()))
  (0 until enqWidth).foreach(w => {
    sram_write_valids.foreach(_:= false.B)
    sram_write_addrs := DontCare
    sram_write_data := DontCare
    sram_fifo.io.writes(w).valid := sram_write_valids(w)
    sram_fifo.io.writes(w).bits.addr := sram_write_addrs(w)
    sram_fifo.io.writes(w).bits.data := sram_write_data(w)
  })

  def forward(int: UInt): UInt = WrapInc(int, numEntries)

  for(i <- 0 until enqWidth){
    assert(previous_valid || !io.enq_uops(i).valid, f"enq($i) has to happen compacted!")
    previous_valid = io.enq_uops(i).valid
    io.enq_uops(i).ready := !mod_valids(enq_tail)
    val tmp_enq_tail = forward(enq_tail)
    when(io.enq_uops(i).fire()){
      // set valid
      next_valids(enq_tail) := true.B
      // write sram
      sram_write_valids(i) := true.B
      sram_write_addrs(i) := enq_tail
      sram_write_data(i) := io.enq_uops(i).bits
      // set br_mask
      br_masks(enq_tail) := io.enq_uops(i).bits.br_mask
      // move tail
      next_tail := tmp_enq_tail
    }
    enq_tail = tmp_enq_tail
  }

  // head advancement logic
  val next_head = WireInit(head)
  // reads for current cycle
  var deq_head = head
  // reads for next cycle
  var read_next_head = next_head
  val sram_read_resp_data = Wire(Vec(deqWidth, new MicroOp()))
  val sram_read_req_addr = Wire(Vec(deqWidth, UInt()))
  (0 until deqWidth).foreach(w => {
    sram_read_req_addr := DontCare
    sram_fifo.io.read_req(w).valid := true.B
    sram_fifo.io.read_req(w).bits.addr := sram_read_req_addr(w)
    sram_read_resp_data(w) := sram_fifo.io.read_resp(w).bits.data
  })
  var previous_fire = true.B
  for(i <- 0 until deqWidth){
    io.heads(i).valid := mod_valids(deq_head)
    // use head_col here because it depends on previous cycle
    io.heads(i).bits := sram_read_resp_data(i)
    // TODO: use mod_br_masks here?
    io.heads(i).bits.br_mask := br_masks(deq_head)
    // sram read
    assert(previous_fire || !io.heads(i).fire(), "heads didn't fire in order")
    previous_fire = previous_fire && io.heads(i).fire()
    sram_read_req_addr(i) := read_next_head
    val tmp_deq_head = forward(deq_head)
    when(io.heads(i).fire()){
      next_valids(deq_head) := false.B
      next_head := tmp_deq_head
    }
    deq_head = tmp_deq_head
    val tmp_read_next_head = forward(read_next_head)
    read_next_head = tmp_read_next_head
  }

  val any_valid = valids.reduce(_||_)
  assert(valids(head) || !any_valid, "head doesn't point to correct spot!")

  head := next_head
  tail := next_tail
  valids := next_valids

  when(io.flush){
    mod_valids.foreach(_ := false.B)
    valids.foreach(_ := false.B)
    head := head
    tail := head
  }
}

class InternalMultiSramDispatchQueue(params: DispatchQueueParams,
                                         )(implicit p: Parameters) extends DispatchQueue(params.numEntries, params.deqWidth, params.enqWidth, params.qName, params.stallOnUse)
{
  require(stallOnUse, "InternalSramDispatchQueue relies on stall on use!")
  val sram_fifo = (0 until fifoWidth).map(i => Module(new UopSram(numEntries, 1, 1)))

  // Branch mask and valid bits are still stored in Regs
  val totalEntires = numEntries*fifoWidth

  val br_masks = Reg(Vec(numEntries, Vec(fifoWidth,UInt(maxBrCount.W) )))
  val valids = RegInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(fifoWidth)(false.B)))))
  val head_row = RegInit(0.U(qAddrSz.W))
  val head_col = RegInit(0.U(qWidthSz.W))
  val tail_row = RegInit(0.U(qAddrSz.W))
  val tail_col = RegInit(0.U(qWidthSz.W))

  def forward(row: UInt, col: UInt): (UInt, UInt) = {
    val row_wrap = row === (numEntries-1).U
    val col_wrap = col === (fifoWidth-1).U
    val new_col = Mux(col_wrap, 0.U(qWidthSz.W), col + 1.U)
    val new_row = Mux(col_wrap, Mux(row_wrap, 0.U(qAddrSz.W), row + 1.U), row)
    (new_row, new_col)
  }

  val mod_valids = WireInit(valids)
  val mod_br_masks = WireInit(br_masks)

  // branch update
  when(io.brinfo.valid) {
    for (r <- 0 until numEntries) {
      for (c <- 0 until fifoWidth) {
        val br_mask = br_masks(r)(c)
        val entry_match = valids(r)(c) && maskMatch(io.brinfo.mask, br_mask)
        when (entry_match && io.brinfo.mispredict) { // Mispredict
          mod_valids(r)(c) := false.B
        }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
          mod_br_masks(r)(c) := (br_mask & ~io.brinfo.mask)
        }
      }
    }
  }
  // up here so writes can go directly to br_masks below
  br_masks := mod_br_masks

  // update tail - branch
  val mod_tail_row = WireInit(tail_row)
  val mod_tail_col = WireInit(tail_col)
  // go through from the back - relies on there being one continuous section of valids
  var (previous_r, previous_c) = (0,0)
  var transitions_mod_valids: List[Bool] = Nil
  // use Mux1H to prevent expensive long priority mux
  val mod_tail_mux_selector = new ListBuffer[Bool];
  val mod_tail_mux_row = new ListBuffer[UInt];
  val mod_tail_mux_col = new ListBuffer[UInt];
  for (r <- (0 until numEntries).reverse) {
    for (c <- (0 until fifoWidth).reverse) {
      mod_tail_mux_selector += mod_valids(r)(c) && !mod_valids(previous_r)(previous_c)
      mod_tail_mux_row += previous_r.U
      mod_tail_mux_col += previous_c.U

      transitions_mod_valids = (mod_valids(r)(c) && !mod_valids(previous_r)(previous_c)) :: transitions_mod_valids
      previous_r = r
      previous_c = c
    }
  }
  assert(PopCount(transitions_mod_valids) <= 1.U, f"holes in mod_valids of $qName!")
  // noting valid - set to head
  when(!mod_valids.map(r => r.reduce(_||_)).reduce(_||_)){
    mod_tail_row := head_row
    mod_tail_col := head_col
  }.otherwise{
    mod_tail_row := Mux1H(mod_tail_mux_selector, mod_tail_mux_row)
    mod_tail_col := Mux1H(mod_tail_mux_selector, mod_tail_mux_col)
  }

  val next_valids = WireInit(mod_valids)
  // update tail - enq
  val next_tail_row = WireInit(mod_tail_row)
  val next_tail_col = WireInit(mod_tail_col)
  var (enq_tail_row, enq_tail_col) = (mod_tail_row, mod_tail_col)
  var previous_valid = true.B
  val sram_write_valids = Wire(Vec(fifoWidth, Bool()))
  val sram_write_addrs = Wire(Vec(fifoWidth, UInt()))
  val sram_write_data = Wire(Vec(fifoWidth, new MicroOp()))
  (0 until fifoWidth).foreach(w => {
    sram_write_valids.foreach(_:= false.B)
    sram_write_addrs := DontCare
    sram_write_data := DontCare
    sram_fifo(w).io.writes(0).valid := sram_write_valids(w)
    sram_fifo(w).io.writes(0).bits.addr := sram_write_addrs(w)
    sram_fifo(w).io.writes(0).bits.data := sram_write_data(w)
  })
  for(i <- 0 until enqWidth){
    assert(previous_valid || !io.enq_uops(i).valid, f"enq($i) has to happen compacted!")
    previous_valid = io.enq_uops(i).valid
    io.enq_uops(i).ready := !mod_valids(enq_tail_row)(enq_tail_col)
    val (tmp_enq_tail_row, tmp_enq_tail_col) = forward(enq_tail_row, enq_tail_col)
    when(io.enq_uops(i).fire()){
      // set valid
      next_valids(enq_tail_row)(enq_tail_col) := true.B
      // write sram
      sram_write_valids(enq_tail_col) := true.B
      sram_write_addrs(enq_tail_col) := enq_tail_row
      sram_write_data(enq_tail_col) := io.enq_uops(i).bits
      // set br_mask
      br_masks(enq_tail_row)(enq_tail_col) := io.enq_uops(i).bits.br_mask
      // move tail
      next_tail_row := tmp_enq_tail_row
      next_tail_col := tmp_enq_tail_col
    }
    enq_tail_row = tmp_enq_tail_row
    enq_tail_col = tmp_enq_tail_col
  }

  // head advancement logic
  val next_head_row = WireInit(head_row)
  val next_head_col = WireInit(head_col)
  // reads for current cycle
  var (deq_head_row, deq_head_col) = (head_row, head_col)
  // reads for next cycle
  var (read_next_head_row, read_next_head_col) = (next_head_row, next_head_col)
  val sram_read_resp_data = Wire(Vec(fifoWidth, new MicroOp()))
  val sram_read_req_addr = Wire(Vec(fifoWidth, UInt()))
  (0 until fifoWidth).foreach(w => {
    sram_read_req_addr := DontCare
    sram_fifo(w).io.read_req(0).valid := true.B
    sram_fifo(w).io.read_req(0).bits.addr := sram_read_req_addr(w)
    sram_read_resp_data(w) := sram_fifo(w).io.read_resp(0).bits.data
  })
  var previous_fire = true.B
  for(i <- 0 until deqWidth){
    io.heads(i).valid := mod_valids(deq_head_row)(deq_head_col)
    // use head_col here because it depends on previous cycle
    io.heads(i).bits := sram_read_resp_data(deq_head_col)
    // TODO: use mod_br_masks here?
    io.heads(i).bits.br_mask := br_masks(deq_head_row)(deq_head_col)
    // sram read
    assert(previous_fire || !io.heads(i).fire(), "heads didn't fire in order")
    previous_fire = previous_fire && io.heads(i).fire()
    sram_read_req_addr(read_next_head_col) := read_next_head_row
    dontTouch(WireInit(read_next_head_row).suggestName(f"read_next_head_row_$i"))
    dontTouch(WireInit(read_next_head_col).suggestName(f"read_next_head_col_$i"))
    val (tmp_deq_head_row, tmp_deq_head_col) = forward(deq_head_row, deq_head_col)
    when(io.heads(i).fire()){
      next_valids(deq_head_row)(deq_head_col) := false.B
      next_head_row := tmp_deq_head_row
      next_head_col := tmp_deq_head_col
    }
    deq_head_row = tmp_deq_head_row
    deq_head_col = tmp_deq_head_col
    val (tmp_read_next_head_row, tmp_read_next_head_col) = forward(read_next_head_row, read_next_head_col)
    read_next_head_row = tmp_read_next_head_row
    read_next_head_col = tmp_read_next_head_col
  }

  val any_valid = valids.map(_.reduce(_||_)).reduce(_||_)
  assert(valids(head_row)(head_col) || !any_valid, "head doesn't point to correct spot!")

  head_row := next_head_row
  head_col := next_head_col
  tail_row := next_tail_row
  tail_col := next_tail_col
  valids := next_valids

  when(io.flush){
    mod_valids.foreach(r=>r.foreach(_ := false.B))
    valids.foreach(r=>r.foreach(_ := false.B))
    head_row := head_row
    head_col := head_col
    tail_row := head_row
    tail_col := head_col
  }
}

class SramDispatchQueueCompactingShifting(params: DispatchQueueParams,
                                         )(implicit p: Parameters) extends DispatchQueue(params.numEntries, params.deqWidth, params.enqWidth, params.qName, params.stallOnUse)
{

//  val sram_fifo = (0 until fifoWidth).map(i => SyncReadMem(numEntries, new MicroOp))
  val sram_fifo = (0 until fifoWidth).map(i => Module(new UopSram(numEntries, 1, 1)))

  // Branch mask and valid bits are still stored in Regs
  val br_mask = Reg(Vec(numEntries, Vec(fifoWidth,UInt(maxBrCount.W) )))
  val valids = RegInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(fifoWidth)(false.B)))))
  val head_row = RegInit(0.U(qAddrSz.W))
  val head_col = RegInit(0.U(qWidthSz.W))
  val tail_row = RegInit(0.U(qAddrSz.W))
  val tail_col = RegInit(0.U(qWidthSz.W))
  val full = WireInit(false.B)
  val empty = WireInit(true.B)

  // To support multiple dequeues we need to decouple the heads from the queues and use regs
  val heads_uop = Reg(Vec(deqWidth, new MicroOp()))
  val heads_brmask = Reg(Vec(deqWidth, UInt(maxBrCount.W)))
  val heads_valid = RegInit(VecInit(Seq.fill(deqWidth)(false.B)))
  // heads_out wires are the outputs of the regs. They are fed to the io.heads and also back into the
  //  regs as they are shifted towards lower index when we have dequeues.
  val heads_out_uop = Wire(Vec(deqWidth, new MicroOp()))
  val heads_out_valid = Wire(Vec(deqWidth, Bool()))
  val heads_out_brmask = Wire(Vec(deqWidth, UInt(maxBrCount.W)))

  // Wires for bypassing enq->heads
  val bypass_valids = WireInit(VecInit(Seq.fill(deqWidth)(false.B)))
  val bypass_uops = Wire(Vec(deqWidth, new MicroOp()))
  bypass_uops.map(_ := DontCare)

  // Stage 1 enqueues stored for bypassing.
  val s1_enq_uops = Reg(Vec(fifoWidth, new MicroOp()))
  val s1_enq_valids = RegInit(VecInit(Seq.fill(fifoWidth)(false.B)))
  val s1_enq_row = RegInit(VecInit(Seq.fill(fifoWidth)(0.U(qAddrSz.W))))

  // Stage 1 sram write wires.
  val s1_write_uops = Wire(Vec(fifoWidth, new MicroOp()))
  val s1_write_rows = WireInit(VecInit(Seq.fill(fifoWidth)(0.U(qAddrSz.W))))
  val s1_write_valids = WireInit(VecInit(Seq.fill(fifoWidth)(false.B)))

  s1_write_uops.map(_ := DontCare)


  // Stage 2 read-outs from SRAM
  val s2_sram_read_uop = Wire(Vec(fifoWidth, new MicroOp()))
  val s2_sram_read_idx = RegInit(VecInit(Seq.fill(fifoWidth)(0.U(qWidthSz.W))))
  val s2_sram_read_idx_map = RegInit(VecInit(Seq.fill(fifoWidth)(0.U(qWidthSz.W))))
  val s2_sram_read_row = RegInit(VecInit(Seq.fill(fifoWidth)(0.U(qAddrSz.W))))
  val s2_sram_read_valids = RegInit(VecInit(Seq.fill(fifoWidth)(false.B)))
  dontTouch(s2_sram_read_valids)
  dontTouch(s2_sram_read_uop)

  // Wires for calculating state in next CC
  val head_row_next = WireInit(head_row)
  val head_col_next = Wire(UInt(qWidthSz.W))
  head_col_next := head_col
  val tail_row_next = WireInit(tail_row)
  val tail_col_next = WireInit(tail_col)
  val deqs = WireInit(VecInit(Seq.fill(fifoWidth)(false.B)))

  // Wires for branch resolutions
  val updated_brmask = WireInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(fifoWidth)(false.B))))) //This wire decides if we should block the deque from head because of a branch resolution
  val entry_killed = WireInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(fifoWidth)(false.B)))))

  // Connect heads_out to heads and pass them out to io.heads
  (heads_out_uop zip heads_uop).map {case (out, reg) => out := reg}
  (heads_out_valid zip heads_valid).map {case (out, reg) => out := reg}
  (heads_out_brmask zip heads_brmask).map {case (out, reg) => out := reg}





  // Bypass enq->head when
  //  1. SRAM is empty and there are no valids at head == FIFO is completely empty
  //  2. SRAM is empty and the valid heads are also fired this CC == FIFO will be empty next CC
  val heads_will_be_valid = (heads_out_valid zip io.heads).map {case (l,r) => l && !r.fire}
  val bypass_head =   !heads_will_be_valid.reduce(_ || _) && empty

  dontTouch(bypass_head)
  // enqs_bypassed are used to count how many uops were bypassed and also calculate the idx into heads() to place them
  val enqs_bypassed = WireInit(VecInit(Seq.fill(enqWidth)(false.B)))
  when(bypass_head) {
    // Add enqs directly to the head
    // NB: Watch out as we ofteen have more enqueues than dequeues. Only add the first n to heads and then the rest
    // to the sram. (Which often will be bypassed also)

    // last_bypass_idx is tracking which enqs where bypassed to the head. Needed later
    //  when remaining enq are added to the SRAM

    val last_bypass_idx = WireInit(0.U((qWidthSz+1).W))
    dontTouch(last_bypass_idx)
    for (w <- 0 until enqWidth) {
      // deqIdx is the dequeue index or the head index where this enq will go
      //  Since we dont always have ordered enqs (maybe only enq_2 and not enq_0 or enq_1
      //  we have to calculate this
      val deqIdx = Wire(UInt(qWidthSz.W))
      if (w == 0) {
        deqIdx := 0.U
      } else {
        deqIdx := PopCount(enqs_bypassed.slice(0,w))
      }
      // If this enq is valid and fires, we store it in the bypass-wires
      when(io.enq_uops(w).fire && deqIdx < deqWidth.U) {
        enqs_bypassed(w) := true.B
        bypass_valids(deqIdx) := true.B
        bypass_uops(deqIdx) := io.enq_uops(w).bits
        last_bypass_idx := w.U
      }
    }
    // Little hacking to get the index into io_enqs where to start adding
    //  to the SRAM. Need the index as a ScalaInt to have it in a for loop
    var start_remaining_scalaInt = -1 // Initialize to something invalid because I had a bug where it never got assigned
    for(i <- 0 until enqWidth + 1){
      when(i.U === last_bypass_idx + 1.U) {
        start_remaining_scalaInt = i
      }
    }
    // enqs track how many enqs are added to the SRAM so we can calculate where to
    //  put the next
    val enqs = WireInit(VecInit(Seq.fill(enqWidth)(false.B)))
    // Add remaining uops to the SRAM
    //  start_remaining is the io_enq idx to start iteration from
    //  The col and row where the uop is stored is based on how many has been enqueued
    //  since the start_remaining. The others are directly bypassed.
    for( w <- start_remaining_scalaInt until enqWidth) {
      val i = w - start_remaining_scalaInt
      val enq_idx = Wire(UInt(qWidthSz.W))
      if (i == 0) {
        enq_idx := 0.U
      } else {
        enq_idx := PopCount(enqs.slice(0,i))
      }

      val col = WrapAdd(tail_col, enq_idx, fifoWidth)
      val row = Mux(col < tail_col, WrapInc(tail_row, numEntries), tail_row)

      when(io.enq_uops(w).fire) {
        s1_write_rows(col) := row
        s1_write_uops(col) := io.enq_uops(w).bits
        s1_write_valids(col) := true.B

        enqs(w) := true.B
      }

    }

  }.otherwise {
    // Normal case of enqueing to a half-full FIFO
    val enqs = WireInit(VecInit(Seq.fill(enqWidth)(false.B)))
    for (w <- 0 until enqWidth) {
      val enq_idx = Wire(UInt(qWidthSz.W))
      if (w == 0) {
        enq_idx := 0.U
      } else {
        enq_idx := PopCount(enqs.slice(0,w))
      }

      val col = WrapAdd(tail_col, enq_idx, fifoWidth)
      val row = Mux(col < tail_col, WrapInc(tail_row, numEntries), tail_row)

      when(io.enq_uops(w).fire) {

        s1_write_uops(col) := io.enq_uops(w).bits
        s1_write_valids(col) := true.B
        s1_write_rows(col) := row

        enqs(w) := true.B
      }
    }
  }

  // Do the actual SRAM writes
  for (i <- 0 until fifoWidth) {
    sram_fifo(i).io.writes(0).valid := s1_write_valids(i)
    sram_fifo(i).io.writes(0).bits.addr := s1_write_rows(i)
    sram_fifo(i).io.writes(0).bits.data := s1_write_uops(i)
    when (s1_write_valids(i)) {
//      sram_fifo(i).write(s1_write_rows(i), s1_write_uops(i))

      valids(s1_write_rows(i))(i) := true.B
      br_mask(s1_write_rows(i))(i) := s1_write_uops(i).br_mask

      // Here we assume that we dont try to enqueue anything to a full queue
      assert(!valids(s1_write_rows(i))(i), "[dis-q] tyring to enqueue to a full queue")
    }
    s1_enq_uops(i) := s1_write_uops(i)
    s1_enq_valids(i) := s1_write_valids(i)
    s1_enq_row(i) := s1_write_rows(i)

  }

  // Read from SRAM
  //  Each CC we will read out the value of head+1 .. head+nDeqs. Next CC they will be MUX'ed into heads regs
  //  together with bypasses etc. We read an entire row eac CC. COlumn 0 always goes into
  //  s2_sram_read_0.
  for(i <- 0 until fifoWidth) {
    // Idx is the order of this column. s2_sram_read_0 always contains the uop in
    //  column 0. But as the head moves we need to calculate which order it has.
    //  We need this when we shift them into the head register later. Uops closest to head
    //  are oldest and get priority. To quickly read out the sram reads after their priority
    //  s2_sram_idx_map is a mapping from priority -> index into s2_sram_read.
    //  if s2_sram_idx_map = (2,0,1) Then highest priority uop is in s2_sram_read_uop(2) then s2_sram_read_uop(0) and so on
    val idx_map = WrapSubUInt(i.U, head_col_next, fifoWidth)
    val row = Wire(UInt(qAddrSz.W))
    // Calculate row. either its the head row or, if we have a wrap-around, its the next
    row := Mux(idx_map > i.U, WrapInc(head_row_next, numEntries), head_row_next)
    s2_sram_read_row(i) := row
    s2_sram_read_idx_map(idx_map) := i.U
//    s2_sram_read_uop(i) := sram_fifo(i)(row) // This is the actual reading
    sram_fifo(i).io.read_req(0).valid := true.B
    sram_fifo(i).io.read_req(0).bits.addr := row
    s2_sram_read_uop(i) := sram_fifo(i).io.read_resp(0).bits.data
    s2_sram_read_valids(i) := valids(i)(row)
  }

  // Pass out the heads. Here we connect the io to the head registers
  var prev_fire = WireInit(true.B)
  for (i <- 0 until deqWidth) {
    io.heads(i).bits := heads_out_uop(i)
    io.heads(i).bits.br_mask := heads_out_brmask(i)
    if(stallOnUse){
      io.heads(i).valid := heads_out_valid(i) && prev_fire
      prev_fire = io.heads(i).fire()
    } else{
      io.heads(i).valid := heads_out_valid(i)
    }


    // If we dequeue this head, update state to false next CC. This false will be overwritten more down if we
    //  refill the head
    when(io.heads(i).fire) {
      heads_valid(i) := false.B
      assert(heads_valid(i), "[dis-q] Dequeued invalid uop from head")
      assert(!io.flush, "[dis-q] Fired head during flush")
    }
    if (i > 0) {
      assert(!(io.heads(i).valid && !io.heads.slice(0,i).map(_.valid).reduce(_ && _)), "[dis-q] compact heads")
    }
  }


  // Now we have to shift and refill the heads.
  //  First we shift the remaining heads towards index 0 to keep it age.based

  for (i <- 0 until deqWidth) {
    val heads_idx = Wire(UInt(qWidthSz.W))
    if( i == 0) {
      heads_idx := 0.U
    } else {
      heads_idx := PopCount(heads_will_be_valid.slice(0,i))
    }
    when(heads_will_be_valid(i)) {
      heads_uop(heads_idx) := heads_out_uop(i)
      heads_brmask(heads_idx) := heads_out_brmask(i)
      heads_valid(heads_idx) := heads_out_valid(i)

      assert(heads_out_valid(i), "[dis-q] shifting invalid/killed head")
    }
  }

  // Then we refill the free head slots. The refills can come from
  //  1. Bypass-wires. If SRAM is empty and heads are all empty og is fired this CC
  //    any enqueud uop will be bypassed through the bypass-wires
  //  2. s1_enq_uop: If SRAM was empty last CC but heads were not. We will have to do the
  //    second level bypass through the s1_enq regs
  //  3. s2_sram_read: The normal case where we refill the heads with the readout from the SRAM
  val refills_needed = deqWidth.U - PopCount(heads_will_be_valid)
  for (i <- 0 until deqWidth) {
    when (i.U < refills_needed) {
      // heads_idx = what is the index into the head regs where this uop will be placed
      val heads_idx = deqWidth.U - refills_needed + i.U
      val col = s2_sram_read_idx_map(i) // What is the column of the uop in the SRAM
      val row = Wire(UInt(qAddrSz.W)) // What is the row of the uop in the SRAM
      row := s2_sram_read_row(col)
      when (!io.flush) {
        val uop = WireInit(Mux(bypass_valids(i), bypass_uops(i), s2_sram_read_uop(col)))
        val valid = Mux(bypass_valids(i), true.B, valids(row)(col) && !entry_killed(row)(col))
        val brmask = Mux(bypass_valids(i), bypass_uops(i).br_mask, Mux(!updated_brmask(row)(col),
          br_mask(row)(col),
          br_mask(row)(col) & ~io.brinfo.mask)
        )

        for (j <- 0 until fifoWidth) {
          when(row === s1_enq_row(j) && col === j.U && s1_enq_valids(j)) {
            uop := s1_enq_uops(j)
            assert(!bypass_valids(i), "[dis-q-comp] S1 bypass and head bypass on same uop")
          }
        }
        uop.br_mask := brmask
        heads_uop(heads_idx) := uop
        heads_brmask(heads_idx) := brmask
        heads_valid(heads_idx) := valid

        // Deqs are used to count how many SRAM dequeues we have so we know how much to move
        //  the head ptrs
        deqs(i) := Mux(bypass_valids(i), false.B, valids(row)(col) && !entry_killed(row)(col))
        when (deqs(i) && valids(row)(col)) {
          valids(row)(col) := false.B
        }
      }
    }
  }

  assert(!( RegNext((head_col_next =/= head_col)) && valids(RegNext(head_row))(RegNext(head_col))), "[dis-q] Head ptr moved but SRAM not freed up")

  // Calculate next head
  val nDeqs = PopCount(deqs)

  head_col_next := WrapAdd(head_col, nDeqs, fifoWidth)
  head_row_next := Mux(head_col_next < head_col || nDeqs === fifoWidth.U, WrapInc(head_row, numEntries), head_row)


  // Update tail
  //  We only update the tail for the next CC if there was a fire AND it wasnt bypassed


  val tail_move_by = PopCount((io.enq_uops zip enqs_bypassed).map {case(l,r) => l.fire && !r})
  tail_col_next := WrapAdd(tail_col, tail_move_by, fifoWidth)
  tail_row_next := Mux(tail_col_next < tail_col || tail_move_by === fifoWidth.U , WrapInc(tail_row, numEntries), tail_row)

  assert(!(!valids(head_row)(head_col) && !empty), "[dis-q] head is invalid but we are not empty")







  // Only allow enqueues when we have room for enqWidth enqueus this CC
  for (w <- 0 until enqWidth)
  {
    io.enq_uops(w).ready := !full
  }





  // Full/Empty
  //  We are full when we have less than enqWidth free slots
  when( head_row === tail_row) {
    full := true.B
    empty := false.B
    for (i <- 0 until numEntries) {
      when(!valids(i).reduce(_||_)) {
        full := false.B
        when(head_col === tail_col) {
          empty := true.B
        }
      }
    }
  }.elsewhen( ((WrapInc(tail_row, numEntries) === head_row) && (head_col < tail_col)) ) {
    when(valids(head_row)(head_col)) {
      full := true.B
      empty := false.B
    }.otherwise {
      full := false.B
      empty := false.B
      assert(false.B, "[dis-q] full/empty logic should never get here")
    }
  }.otherwise {
    full := false.B
    empty := false.B
  }


  // Handle branch resolution
  //  On mispredict, find oldest that is killed and kill everyone younger than that
  //  On resolved. Update all branch masks in paralell. Updates also invalid entries, for simplicity.
  //  Since heads are in separate Regs they must also be searched through
  when(io.brinfo.valid) {
    for (idx <- 0 until numEntries) {
      for (lane <- 0 until fifoWidth) {
        val entry_match = valids(idx)(lane) && maskMatch(io.brinfo.mask, br_mask(idx)(lane))
        when(entry_match && io.brinfo.mispredict) { // Mispredict
          entry_killed(idx)(lane) := true.B
          valids(idx)(lane) := false.B
        }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
          br_mask(idx)(lane) := (br_mask(idx)(lane) & ~io.brinfo.mask)
          updated_brmask(idx)(lane) := true.B
        }
      }
    }

    for (idx <- 0 until deqWidth) {
      val entry_match = heads_valid(idx) && maskMatch(io.brinfo.mask, heads_brmask(idx))
      when(entry_match && io.brinfo.mispredict) {
        //       heads_killed(idx) := true.B
        heads_out_valid(idx) := false.B
      }.elsewhen(entry_match && !io.brinfo.mispredict) {
        //      heads_updated_brmask(idx) := true.B
        heads_out_brmask(idx) := heads_brmask(idx) & ~io.brinfo.mask
        heads_out_uop(idx).br_mask := heads_brmask(idx) & ~io.brinfo.mask
      }
    }


    // tail update logic
    for (i <- 0 until numEntries) {
      for (j <- 0 until fifoWidth) {
        val previous_killed =
          if (i == 0) {
            if (j == 0) {
              entry_killed(numEntries - 1)(fifoWidth - 1)
            } else {
              entry_killed(i)(j - 1)
            }
          } else {
            if (j == 0) {
              entry_killed(i - 1)(fifoWidth - 1)
            } else {
              entry_killed(i)(j - 1)
            }
          }
        // transition from not killed to killed - there should be one at maximum
        when(!previous_killed && entry_killed(i)(j)) {
          // this one was killed but the previous one not => this is tail
          // if branches are killed there should be nothing being enqueued
          tail_row_next := i.U
          tail_col_next := j.U
        }
      }
    }
  }


  assert(! (io.brinfo.mispredict && io.brinfo.valid &&  entry_killed(tail_row)(tail_col) && ((head_row_next =/= tail_row_next) || (head_col_next =/= tail_col_next))), "[dis-q] branch resolution with head flushed but head and tail_next not reset")
  assert(! (RegNext(io.brinfo.mispredict && io.brinfo.valid && entry_killed(tail_row)(tail_col)) && (full =/= false.B || empty =/= true.B )), "[dis-q] branch resolution with head flushed but empty, full not reset")


  // Pipline flushes
  when(io.flush)
  {
    head_row_next := 0.U
    head_col_next := 0.U
    tail_row_next := 0.U
    tail_col_next := 0.U
    valids.map(_.map(_ := false.B))
    heads_valid.map(_ := false.B)
    io.heads.map(_.valid := false.B)
  }

  // Update for next CC
  head_row := head_row_next
  head_col := head_col_next
  tail_row := tail_row_next
  tail_col := tail_col_next


  if (O3PIPEVIEW_PRINTF) {
    when(io.tsc_reg>=O3_START_CYCLE.U) {
      for (i <- 0 until enqWidth) {
        when(io.enq_uops(i).valid) {
          printf("%d; O3PipeView:" + qName + ": %d\n",
            io.enq_uops(i).bits.debug_events.fetch_seq,
            io.tsc_reg)
        }
      }
    }
  }

  dontTouch(io)
  dontTouch(valids)
  dontTouch(head_row)
  dontTouch(tail_row)
  dontTouch(head_row_next)
  dontTouch(tail_row_next)
  dontTouch(full)
  dontTouch(empty)
}



class SramDispatchQueueCompacting(params: DispatchQueueParams,
                                 )(implicit p: Parameters) extends DispatchQueue(params.numEntries, params.deqWidth, params.enqWidth, params.qName)
{
  // enqWidth has to be bigger than deqWidth
  require(enqWidth >= deqWidth)

  val sram_fifo = (0 until enqWidth).map(i => SyncReadMem(numEntries, new MicroOp))

  // Branch mask and valid bits are still stored in Regs
  val br_mask = Reg(Vec(numEntries, Vec(enqWidth,UInt(maxBrCount.W) )))
  val valids = RegInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(enqWidth)(false.B)))))
  val head_row = RegInit(0.U(qAddrSz.W))
  val head_col = RegInit(0.U(qWidthSz.W))
  val tail_row = RegInit(0.U(qAddrSz.W))
  val tail_col = RegInit(0.U(qWidthSz.W))
  val full = WireInit(false.B)
  val empty = WireInit(true.B)

  // To support multiple dequeues we need to decouple the heads from the queues and use regs
  val heads_uop = Reg(Vec(deqWidth, new MicroOp()))
  val heads_brmask = Reg(Vec(deqWidth, UInt(maxBrCount.W)))
  val heads_valid = RegInit(VecInit(Seq.fill(deqWidth)(false.B)))

  // Wires for bypassing enq->heads
  val bypass_valids = WireInit(VecInit(Seq.fill(deqWidth)(false.B)))
  val bypass_uops = Wire(Vec(deqWidth, new MicroOp()))
  bypass_uops.map(_ := DontCare)

  // Stage 1 enqueues stored for bypassing.
  val s1_enq_uops = Reg(Vec(enqWidth, new MicroOp()))
  val s1_enq_valids = RegInit(VecInit(Seq.fill(enqWidth)(false.B)))
  val s1_enq_row = RegInit(VecInit(Seq.fill(enqWidth)(0.U(qAddrSz.W))))
  val s1_enq_col = RegInit(VecInit(Seq.fill(enqWidth)(0.U(qWidthSz.W))))

  // Stage 2 read-outs from SRAM
  val s2_sram_read_uop = Wire(Vec(enqWidth, new MicroOp()))
  val s2_sram_read_idx = RegInit(VecInit(Seq.fill(enqWidth)(0.U(qWidthSz.W))))
  val s2_sram_read_idx_map = RegInit(VecInit(Seq.fill(enqWidth)(0.U(qWidthSz.W))))
  val s2_sram_read_row = RegInit(VecInit(Seq.fill(enqWidth)(0.U(qAddrSz.W))))
  val s2_sram_read_valids = RegInit(VecInit(Seq.fill(enqWidth)(false.B)))
  dontTouch(s2_sram_read_valids)
  dontTouch(s2_sram_read_uop)

  // Wires for calculating state in next CC
  val head_row_next = WireInit(head_row)
  val head_col_next = WireInit(head_col)
  val tail_row_next = WireInit(tail_row)
  val tail_col_next = WireInit(tail_col)
  val deqs = WireInit(VecInit(Seq.fill(enqWidth)(false.B)))

  // Wires for branch resolutions
  val updated_brmask = WireInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(enqWidth)(false.B))))) //This wire decides if we should block the deque from head because of a branch resolution
  val entry_killed = WireInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(enqWidth)(false.B)))))
  val heads_killed = WireInit(VecInit(Seq.fill(deqWidth)(false.B)))
  val heads_updated_brmask = WireInit(VecInit(Seq.fill(deqWidth)(false.B)))

  // Update tail
  //  We only update the tail for the next CC if there was a fire AND it wasnt bypassed
  val enqs_bypassed = WireInit(VecInit(Seq.fill(enqWidth)(false.B)))



  assert(!(!valids(head_row)(head_col) && !empty), "[dis-q] head is invalid but we are not empty")

  // Handle branch resolution
  //  On mispredict, find oldest that is killed and kill everyone younger than that
  //  On resolved. Update all branch masks in paralell. Updates also invalid entries, for simplicity.
  //  Since heads are in separate Regs they must also be searched through
  when(io.brinfo.valid) {
    for (idx <- 0 until numEntries) {
      for (lane <- 0 until enqWidth) {
        val entry_match = valids(idx)(lane) && maskMatch(io.brinfo.mask, br_mask(idx)(lane))
        when(entry_match && io.brinfo.mispredict) { // Mispredict
          entry_killed(idx)(lane) := true.B
          valids(idx)(lane) := false.B
        }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
          br_mask(idx)(lane) := (br_mask(idx)(lane) & ~io.brinfo.mask)
          updated_brmask(idx)(lane) := true.B
        }
      }
    }

    for (idx <- 0 until deqWidth) {
      val entry_match = heads_valid(idx) && maskMatch(io.brinfo.mask, heads_brmask(idx))
      when(entry_match && io.brinfo.mispredict) {
        heads_killed(idx) := true.B
        heads_valid(idx) := false.B
      }.elsewhen(entry_match && !io.brinfo.mispredict) {
        heads_updated_brmask(idx) := true.B
        heads_brmask(idx) := heads_brmask(idx) & ~io.brinfo.mask
        heads_uop(idx).br_mask := heads_brmask(idx) & ~io.brinfo.mask
      }
    }


    // tail update logic
    for (i <- 0 until numEntries) {
      for (j <- 0 until enqWidth) {
        val previous_killed =
          if (i == 0) {
            if (j == 0) {
              entry_killed(numEntries - 1)(enqWidth - 1)
            } else {
              entry_killed(i)(j - 1)
            }
          } else {
            if (j == 0) {
              entry_killed(i - 1)(enqWidth - 1)
            } else {
              entry_killed(i)(j - 1)
            }
          }
        // transition from not killed to killed - there should be one at maximum
        when(!previous_killed && entry_killed(i)(j)) {
          // this one was killed but the previous one not => this is tail
          // if branches are killed there should be nothing being enqueued
          tail_row_next := i.U
          tail_col_next := j.U
        }
      }
    }
  }

  // Bypass enq->head when
  //  1. SRAM is empty and there are no valids at head == FIFO is completely empty
  //  2. SRAM is empty and the valid heads are also fired this CC == FIFO will be empty next CC
  val bypass_head =   (!heads_valid.reduce(_ || _) && empty) ||
    ((heads_valid zip io.heads).map{case (l,r) => (l && r.fire || !l)}.reduce(_ && _) && empty)

  dontTouch(bypass_head)
  // enqs_bypassed are used to count how many uops were bypassed and also calculate the idx into heads() to place them
  when(bypass_head) {
    // Add enqs directly to the head
    // NB: Watch out as we ofteen have more enqueues than dequeues. Only add the first n to heads and then the rest
    // to the sram. (Which often will be bypassed also)

    val last_bypass_idx = WireInit(0.U((qWidthSz+1).W))
    dontTouch(last_bypass_idx)
    for (w <- 0 until enqWidth) {
      val deqIdx = Wire(UInt(qWidthSz.W))
      if (w == 0) {
        deqIdx := 0.U
      } else {
        deqIdx := PopCount(enqs_bypassed.slice(0,w))
      }

      when(io.enq_uops(w).fire && deqIdx < deqWidth.U) {
        enqs_bypassed(w) := true.B
        bypass_valids(deqIdx) := true.B
        bypass_uops(deqIdx) := io.enq_uops(w).bits
        last_bypass_idx := w.U
      }
    }
    var start_remaining_scalaInt = -1 // Initialize to something invalid because I had a bug where it never got assigned
    val enqs = WireInit(VecInit(Seq.fill(enqWidth)(false.B)))
    for(i <- 0 until enqWidth + 1){
      when(i.U === last_bypass_idx + 1.U) {
        start_remaining_scalaInt = i
      }
    }
    // Add remaining uops to the SRAM
    //  start_remaining is the io_enq idx to start iteration from
    //  The col and row where the uop is stored is based on how many has been enqueued
    //  since the start_remaining. The others are directly bypassed.
    for( w <- start_remaining_scalaInt until enqWidth) {
      val i = w - start_remaining_scalaInt
      val enq_idx = Wire(UInt(qWidthSz.W))
      if (i == 0) {
        enq_idx := 0.U
      } else {
        enq_idx := PopCount(enqs.slice(0,i))
      }

      val col = WrapAdd(tail_col, enq_idx, enqWidth)
      val row = Mux(col < tail_col, WrapInc(tail_row, numEntries), tail_row)

      when(io.enq_uops(w).fire) {
        // DavidHack to access FIFO columns with scalaInts
        for(i <- 0 until enqWidth){
          when(i.U === col){
            sram_fifo(i).write(row, WireInit(io.enq_uops(w).bits))
          }
        }
        valids(row)(col) := io.enq_uops(w).valid
        br_mask(row)(col) := io.enq_uops(w).bits.br_mask

        // Here we assume that we dont try to enqueue anything to a full queue
        assert(!valids(row)(col), "[dis-q] tyring to enqueue to a full queue")

        enqs(i) := true.B
      }

      // Latch these for potential bypass
      s1_enq_uops(w) := io.enq_uops(w).bits
      s1_enq_valids(w) := io.enq_uops(w).fire
      s1_enq_row(w) := row
      s1_enq_col(w) := col
    }

  }.otherwise {
    // Normal case of enqueing to a half-full FIFO
    val enqs = WireInit(VecInit(Seq.fill(enqWidth)(false.B)))
    for (w <- 0 until enqWidth) {
      val enq_idx = Wire(UInt(qWidthSz.W))
      if (w == 0) {
        enq_idx := 0.U
      } else {
        enq_idx := PopCount(enqs.slice(0,w))
      }

      val col = WrapAdd(tail_col, enq_idx, enqWidth)
      val row = Mux(col < tail_col, WrapInc(tail_row, numEntries), tail_row)

      when(io.enq_uops(w).fire) {
        // DavidHack to access FIFO columns with scalaInts
        for(i <- 0 until enqWidth){
          when(i.U === col){
            sram_fifo(i).write(row, WireInit(io.enq_uops(w).bits))
          }
        }
        valids(row)(col) := io.enq_uops(w).valid
        br_mask(row)(col) := io.enq_uops(w).bits.br_mask

        // Here we assume that we dont try to enqueue anything to a full queue
        assert(!valids(row)(col), "[dis-q] tyring to enqueue to a full queue")

        enqs(w) := true.B
      }
      // Latch this for potential bypass
      s1_enq_uops(w) := io.enq_uops(w).bits
      s1_enq_valids(w) := io.enq_uops(w).fire
      s1_enq_row(w) := row
      s1_enq_col(w) := col
    }
  }

  val tail_move_by = PopCount((io.enq_uops zip enqs_bypassed).map {case(l,r) => l.fire && !r})
  tail_col_next := WrapAdd(tail_col, tail_move_by, enqWidth)
  tail_row_next := Mux(tail_col_next < tail_col || tail_move_by === enqWidth.U , WrapInc(tail_row, numEntries), tail_row)


  // Read from SRAM
  //  Each CC we will read out the value of head+1 .. head+nDeqs. Next CC they will be MUX'ed into heads regs
  //  together with bypasses etc.

  for(i <- 0 until enqWidth) {
    val idx = WrapSubUInt(i.U, head_col, enqWidth)
    val row = Wire(UInt(qAddrSz.W))
    row := Mux(idx > i.U, WrapInc(head_row, numEntries), head_row)
    s2_sram_read_row(i) := row
    s2_sram_read_idx(i) := idx
    s2_sram_read_idx_map(idx) := i.U
    s2_sram_read_uop(i) := sram_fifo(i)(row)
    s2_sram_read_valids(i) := valids(i)(row)
  }

  // Pass out the heads
  for (i <- 0 until deqWidth) {
    io.heads(i).bits := heads_uop(i)
    io.heads(i).bits.br_mask := heads_brmask(i)
    io.heads(i).valid := heads_valid(i)

    // Was head killed this CC
    when(heads_killed(i)) {
      io.heads(i).valid := false.B
    }
    // Was head brmask updated this CC
    when(heads_updated_brmask(i)) {
      io.heads(i).bits.br_mask := heads_brmask(i) & ~io.brinfo.mask
    }

    // If we dequeue this head, update state to false next CC. This false will be overwritten more down if we
    //  refill the head
    when(io.heads(i).fire) {
      heads_valid(i) := false.B
      assert(heads_valid(i), "[dis-q] Dequeued invalid uop from head")
      assert(!io.flush, "[dis-q] Fired head during flush")
    }
  }


  // Refill the heads by dequeueing from the FIFO
  for (i <- 0 until deqWidth) {
    // refillIdx is used to calculate which of head, head+1 ... to refill into heads regs.
    //  Only refill invalid head_regs or valid ones that are being fired this CC
    val refillIdx = Wire(UInt(qWidthSz.W))
    if (i == 0) {
      refillIdx := 0.U
    } else {
      refillIdx := PopCount((io.heads.map(_.fire).slice(0,i) zip io.heads.map(_.valid).slice(0,i)).map{ case (l,r) => l || !r })
    }

    // s2_sram_idx is the index into the s2_sram vecs where the refillIdx resides
    val s2_sram_idx = s2_sram_read_idx_map(refillIdx)
    val row = Wire(UInt(qAddrSz.W))
    row := s2_sram_read_row(s2_sram_idx)
    val col = s2_sram_idx // That is equvivalent of the column of the data in the actual SRAM

    // If a head was fired or it was invalid. Then refill that head
    //  Either we get the data from the bypass or from the SRAM. There is also the possibility of the s1_bypass
    //  Which is handled below
    when((io.heads(i).fire || !io.heads(i).valid) && !io.flush) {
      heads_uop(i) := Mux(bypass_valids(i), bypass_uops(i), s2_sram_read_uop(s2_sram_idx))
      heads_valid(i) := Mux(bypass_valids(i), true.B, Mux(!entry_killed(row)(col), valids(row)(col), false.B))
      heads_brmask(i) := Mux(bypass_valids(i), bypass_uops(i).br_mask, Mux(!updated_brmask(row)(col),
        br_mask(row)(col),
        br_mask(row)(col) & ~io.brinfo.mask
      ))
      // Do we need a bypass because this uop was enqueued last cycle and is not read as s2_sram_read_uop?
      //  In that case, just overwrite the heads_uop output. br_mask update is already correct since it is stored in regs and not SRAM
      for (j <- 0 until enqWidth) {
        when(row === s1_enq_row(j) && col === j.U && s1_enq_valids(j)) {
          heads_uop(i) := s1_enq_uops(j)
          assert(!bypass_valids(i), "[dis-q-comp] S1 bypass and head bypass on same uop")
        }
      }

      // Update the refill deqs wire if we dequeue something from the SRAM
      //  and update the valids regs to "free" up the space
      deqs(s2_sram_idx) := Mux(bypass_valids(i), false.B, Mux(!entry_killed(row)(col), valids(row)(col), false.B))
      when (deqs(s2_sram_idx) && valids(row)(col)) {
        valids(row)(col) := false.B
      }
    }
  }

  assert(!( RegNext((head_col_next =/= head_col)) && valids(RegNext(head_row))(RegNext(head_col))), "[dis-q] Head ptr moved but SRAM not freed up")

  // Calculate next head
  val nDeqs = PopCount(deqs)

  head_col_next := WrapAdd(head_col, nDeqs, enqWidth)
  head_row_next := Mux(head_col_next < head_col || nDeqs === enqWidth.U, WrapInc(head_row, numEntries), head_row)


  assert(! (io.brinfo.mispredict && io.brinfo.valid &&  entry_killed(tail_row)(tail_col) && ((head_row_next =/= tail_row_next) || (head_col_next =/= tail_col_next))), "[dis-q] branch resolution with head flushed but head and tail_next not reset")
  assert(! (RegNext(io.brinfo.mispredict && io.brinfo.valid && entry_killed(tail_row)(tail_col)) && (full =/= false.B || empty =/= true.B )), "[dis-q] branch resolution with head flushed but empty, full not reset")


  require(enqWidth>=coreWidth)

  // Only allow enqueues when we have room for enqWidth enqueus this CC
  for (w <- 0 until enqWidth)
  {
    io.enq_uops(w).ready := !full
  }



  // Full/Empty
  //  We are full when we have less than enqWidth free slots
  when( head_row === tail_row) {
    full := true.B
    empty := false.B
    for (i <- 0 until numEntries) {
      when(!valids(i).reduce(_||_)) {
        full := false.B
        when(head_col === tail_col) {
          empty := true.B
        }
      }
    }
  }.elsewhen( ((WrapInc(tail_row, numEntries) === head_row) && (head_col < tail_col)) ) {
    when(valids(head_row)(head_col)) {
      full := true.B
      empty := false.B
    }.otherwise {
      full := false.B
      empty := false.B
      assert(false.B, "[dis-q] full/empty logic should never get here")
    }
  }.otherwise {
    full := false.B
    empty := false.B
  }

  // Pipline flushes
  when(io.flush)
  {
    head_row_next := 0.U
    head_col_next := 0.U
    tail_row_next := 0.U
    tail_col_next := 0.U
    valids.map(_.map(_ := false.B))
    heads_valid.map(_ := false.B)
    io.heads.map(_.valid := false.B)
  }

  // Update for next CC
  head_row := head_row_next
  head_col := head_col_next
  tail_row := tail_row_next
  tail_col := tail_col_next


  if (O3PIPEVIEW_PRINTF) {
    when(io.tsc_reg>=O3_START_CYCLE.U) {
      for (i <- 0 until coreWidth) {
        when(io.enq_uops(i).valid) {
          printf("%d; O3PipeView:" + qName + ": %d\n",
            io.enq_uops(i).bits.debug_events.fetch_seq,
            io.tsc_reg)
        }
      }
    }
  }

  dontTouch(io)
  dontTouch(valids)
  dontTouch(head_row)
  dontTouch(tail_row)
  dontTouch(head_row_next)
  dontTouch(tail_row_next)
  dontTouch(full)
  dontTouch(empty)
}

class SramDispatchQueue (params: DispatchQueueParams,
                        )(implicit p: Parameters) extends DispatchQueue(params.numEntries, params.deqWidth, params.enqWidth, params.qName)
{
  // enqWidth has to be bigger than deqWidth
  require(enqWidth >= deqWidth)

  val sram_fifo = (0 until enqWidth).map(i => SyncReadMem(numEntries, new MicroOp))

  // Branch mask and valid bits are still stored in Regs
  val br_mask = Reg(Vec(numEntries, Vec(enqWidth,UInt(maxBrCount.W) )))
  val valids = RegInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(enqWidth)(false.B)))))
  val head = RegInit(0.U(qAddrSz.W))
  val tail = RegInit(0.U(qAddrSz.W))
  val full = WireInit(false.B)
  val empty = WireInit(true.B)

  // Deqeueue pointer maps dequeue port (io.heads) to FIFO-lane.
  val deq_ptr = WireInit(VecInit(Seq.fill(enqWidth)(0.U(log2Ceil(deqWidth).W))))

  // Stage 1 enqueues stored for bypassing.
  val s1_enq_uops = Reg(Vec(enqWidth, new MicroOp()))
  val s1_enq_valids = Reg(Vec(enqWidth, Bool()))
  val s1_bypass = RegInit(false.B)

  // Stage 2 read-outs from SRAM
  val s2_sram_read = Wire(Vec(enqWidth, new MicroOp()))


  // Wires for calculating state in next CC
  val head_next = WireInit(head) // needs to be initialized here because firrtl can't detect that an assignment condition is always met
  val tail_next = Wire(UInt(qAddrSz.W))

  // Wires for branch resolutions
  val updated_brmask = WireInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(enqWidth)(false.B))))) //This wire decides if we should block the deque from head because of a branch resolution
  val entry_killed = WireInit(VecInit(Seq.fill(numEntries)(VecInit(Seq.fill(enqWidth)(false.B)))))


  // Handle enqueues
  for (w <- 0 until enqWidth) {
    when(io.enq_uops(w).fire) {
      sram_fifo(w)(tail) := WireInit(io.enq_uops(w).bits) //TODO: WireInit necessary?
      valids(tail)(w) := io.enq_uops(w).valid
      br_mask(tail)(w) := io.enq_uops(w).bits.br_mask

      // Here we assume that we dont try to enqueue anything to a full queue
      assert(!valids(tail)(w), "[dis-q] tyring to enqueue to a full queue")
    }
    // Latch this for potential bypass
    s1_enq_uops(w) := io.enq_uops(w).bits
    s1_enq_valids(w) := io.enq_uops(w).valid
  }

  s1_bypass := empty && io.enq_uops.map(_.valid).reduce(_||_) //Bypass next CC if we enqueue to an empty FIFO

  // Update tail
  //  We only update the tail for the next CC if there was a fire
  tail_next := Mux(io.enq_uops.map(_.fire).reduce(_ || _), WrapInc(tail,numEntries), tail)

  // Read from SRAM
  for(i <- 0 until enqWidth) {
    s2_sram_read(i) := sram_fifo(i)(head_next)
  }

  // Pass out the head
  //  Init all outputs to invalids
  //  A little hacking to assign a possible wider set of FIFOs to a narrower set of deq ports
  io.heads.map(_.valid := false.B)
  io.heads.map(_.bits := DontCare) //TODO: Connect to NOP?
  val deqPortUsed = WireInit(VecInit(Seq.fill(enqWidth)(false.B)))
  for(i <- 0 until enqWidth) {
    val idx = Wire(UInt(log2Ceil(enqWidth).W))
    if(i == 0) {
      idx := 0.U
    }
    else {
      idx := PopCount(deqPortUsed.slice(0,i))
    }

    when(valids(head)(i) && idx < deqWidth.U) {
      io.heads(idx).valid :=  valids(head)(i) &&
        !entry_killed(head)(i) &&
        !io.flush

      when(s1_bypass) {

        io.heads(idx).bits := s1_enq_uops(i)
        // If we have a branch resolution this CC we need to update the bypassed instruction as well
        io.heads(idx).bits.br_mask := Mux(updated_brmask(head)(i), br_mask(head)(i) & ~io.brinfo.mask, br_mask(head)(i))


      }.otherwise {
        io.heads(idx).bits := s2_sram_read(i)
        // If we have a branch resolution this cycle. Be sure to pass out the updated Brmask and not the old
        //  TODO: Can this affect the Critical Path?
        io.heads(idx).bits.br_mask := Mux(updated_brmask(head)(i), br_mask(head)(i) & ~io.brinfo.mask, br_mask(head)(i))

      }

      deqPortUsed(i) := true.B
      deq_ptr(idx) := i.U
    }
  }



  // Handle dequeues
  for (i <- 0 until deqWidth) {
    when(io.heads(i).fire) {
      valids(head)(deq_ptr(i)) := false.B
      assert(valids(head)(deq_ptr(i)), "[dis-q] Dequeued invalid uop from head")
      assert(!entry_killed(head)(deq_ptr(i)), "[dis-q] Dequeued killed uop from head")
    }
  }

  // Calculate next head
  when(valids(head).reduce(_ || _)) {
    // Case 1: We entered this CC with some valid uops at head
    //  Either those were dequeded this CC and we move head_pointer or w stay at this head
    //  This double loop is a clonky way of checking it. It checks wether all of the valids at head
    //  also match the ones dequeued this CC
    val proceed = WireInit(true.B)
    for (i <- 0 until enqWidth) {
      val uop_done = WireInit(false.B)
      for(j <- 0 until deqWidth) {
        when(!valids(head)(i) || (io.heads(j).fire && deq_ptr(j) === i.U )) {
          uop_done := true.B
        }
      }
      when(!uop_done) {
        proceed := false.B
      }
    }
    head_next := Mux(proceed, WrapInc(head, numEntries), head)
  }



  // Handle branch resolution
  //  On mispredict, find oldest that is killed and kill everyone younger than that
  //  On resolved. Update all branch masks in paralell. Updates also invalid entries, for simplicity.
  //
  // This function takes a killed and valid Vec and finds out of the whole Vec was killed or is invalid
  def width_killed(k: Vec[Bool], v: Vec[Bool]): Bool = {
    v.reduce(_ || _ ) && (k zip v).map{case (kill, valid) => kill || !valid}.reduce(_&&_)
  }
  when(io.brinfo.valid) {
    for (idx <- 0 until numEntries) {
      for(lane <- 0 until enqWidth) {
        val entry_match = valids(idx)(lane) && maskMatch(io.brinfo.mask, br_mask(idx)(lane))
        when (entry_match && io.brinfo.mispredict) { // Mispredict
          entry_killed(idx)(lane) := true.B
          valids(idx)(lane) := false.B
        }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
          br_mask(idx)(lane) := (br_mask(idx)(lane) & ~io.brinfo.mask)
          updated_brmask(idx)(lane) := true.B
        }
      }
    }


    // tail update logic
    for (i <- 0 until numEntries) {
      // treat it as a circular structure
      val previous_killed =
        if (i == 0) {
          width_killed(entry_killed(numEntries - 1) ,valids(numEntries - 1))
        } else {
          width_killed(entry_killed(i-1) ,valids(i-1))
        }
      val this_killed = width_killed(entry_killed(i) ,valids(i))

      // transition from not killed to killed - there should be one at maximum
      when(!previous_killed && this_killed){
        // this one was killed but the previous one not => this is tail
        // if branches are killed there should be nothing being enqueued
        // TODO: make sure this is true (assert)
        tail_next := i.U
      }
    }
  }

  assert(! (io.brinfo.mispredict && io.brinfo.valid &&  width_killed(entry_killed(head), valids(head)) && (head_next =/= tail_next)), "[dis-q] branch resolution with head flushed but head and tail_next not reset")
  assert(! (RegNext(io.brinfo.mispredict && io.brinfo.valid && width_killed(entry_killed(head), valids(head))) && (full =/= false.B || empty =/= true.B )), "[dis-q] branch resolution with head flushed but empty, full not reset")




  require(enqWidth>=coreWidth)


  for (w <- 0 until enqWidth)
  {
    io.enq_uops(w).ready := !full
  }


  // Full/Empty
  when(head === tail) {
    full := true.B
    empty := false.B
    for (i <- 0 until numEntries) {
      when(!valids(i).reduce(_||_)) {
        full := false.B
        empty := true.B
      }
    }
  }.otherwise {
    full := false.B
    empty := false.B
  }

  // Pipeline flushs
  when(io.flush)
  {
    head_next := 0.U
    tail_next := 0.U
    valids.map(_.map(_ := false.B))
  }

  // Update for next CC
  head := head_next
  tail := tail_next

  if (O3PIPEVIEW_PRINTF) {
    when(io.tsc_reg>=O3_START_CYCLE.U) {
      for (i <- 0 until coreWidth) {
        when(io.enq_uops(i).valid) {
          printf("%d; O3PipeView:" + qName + ": %d\n",
            io.enq_uops(i).bits.debug_events.fetch_seq,
            io.tsc_reg)
        }
      }
    }
  }

  dontTouch(io)
  dontTouch(valids)
  dontTouch(head)
  dontTouch(tail)
  dontTouch(head_next)
  dontTouch(tail_next)
  dontTouch(full)
  dontTouch(empty)
}



@chiselName
class SliceDispatchQueue(params: DispatchQueueParams,
                        )(implicit p: Parameters) extends DispatchQueue(params.numEntries, params.deqWidth, params.enqWidth, params.qName)
{

  require(isPow2(numEntries))
  // Queue state
  val q_uop = Reg(Vec(numEntries, new MicroOp()))
  val valids = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val dequeued = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val head = RegInit(0.U(qAddrSz.W))
  val tail = RegInit(0.U(qAddrSz.W))


  // Wires for calculating state in next CC
  val head_next = WireInit(head) // needs to be initialized here because firrtl can't detect that an assignment condition is always met
  val tail_next = Wire(UInt(qAddrSz.W))

  // Handle enqueues
  //  Use WrapInc to sequentialize an arbitrary number of enqueues.
  val enq_idx = Wire(Vec(coreWidth, UInt(qAddrSz.W)))
  for (w <- 0 until coreWidth) {
    if (w == 0) {
      enq_idx(w) := tail
    } else {
      // Here we calculate the q idx to pass back to the ROB
      enq_idx(w) := Mux(io.enq_uops(w - 1).fire, WrapInc(enq_idx(w - 1), numEntries), enq_idx(w - 1))
    }

    when(io.enq_uops(w).fire)
    {
      valids(enq_idx(w)) := true.B
      q_uop(enq_idx(w)) := io.enq_uops(w).bits
    }
  }
  // Update tail
  //  We have already incremented the tail pointer in the previous loop.
  //  Only needs a last increment if the last enq port also fired
  tail_next := Mux(io.enq_uops(coreWidth - 1).fire, WrapInc(enq_idx(coreWidth - 1), numEntries), enq_idx(coreWidth - 1))


  // Handle branch resolution
  //  On mispredict, find oldest that is killed and kill everyone younger than that
  //  On resolved. Update all branch masks in paralell. Updates also invalid entries, for simplicity.
  val updated_brmask = WireInit(false.B)//VecInit(Seq.fill(numEntries)(false.B))) //This wire decides if we should block the deque from head because of a branch resolution
  val entry_killed = WireInit(VecInit(Seq.fill(numEntries)(false.B)))
  when(io.brinfo.valid) {
    for (i <- 0 until numEntries) {
      val br_mask = q_uop(i).br_mask
      val entry_match = valids(i) && maskMatch(io.brinfo.mask, br_mask)
      val dequeued_match = dequeued(i) && maskMatch(io.brinfo.mask, br_mask)

      when (entry_match && io.brinfo.mispredict) { // Mispredict
        entry_killed(i) := true.B
        valids(i) := false.B
      }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
        q_uop(i).br_mask := (br_mask & ~io.brinfo.mask)
        updated_brmask := true.B
      }
      when(dequeued_match&& io.brinfo.mispredict) { // Mispredict of an already dequeued instruction
        dequeued(i) := false.B
        // ugly hack - but needed to get tail to right position
        entry_killed(i) := true.B
      }
    }
    // tail update logic
    for (i <- 0 until numEntries) {
      // treat it as a circular structure
      val previous_killed = if(i==0) entry_killed(numEntries-1) else entry_killed(i-1)
      // transition from not killed to killed - there should be one at maximum
      when(!previous_killed && entry_killed(i)){
        // this one was killed but the previous one not => this is tail
        // if branches are killed there should be nothing being enqueued
        // TODO: make sure this is true (assert)
        tail_next := i.U
      }
    }
  }

  require(numEntries>2*coreWidth)


  for(i <- 0 until deqWidth){
    for(i <- 0 until deqWidth){
      val idx = head + i.U
      // Route out IO
      io.heads(i).bits := q_uop(idx)
      io.heads(i).valid :=  valids(idx) &&
        !updated_brmask && //TODO: this might lead to poor performance
        !entry_killed(idx) &&
        !io.flush // TODO: handle flush?
      when(io.heads(i).fire()){
        valids(idx) := false.B
        dequeued(idx) := true.B
      }
    }
  }
  for (w <- 0 until coreWidth)
  {
    // don't allow filling dequeud entrys
    io.enq_uops(w).ready := !valids(tail+w.U) && !dequeued(tail+w.U)
  }

  // Handle dequeues - nneds to be lower than head logic
  var previous_deq = true.B
  // one more so we also do something if all are dequeued
  for (i <- 0 until deqWidth+1) {
    val current_deq = if(i==deqWidth) false.B else io.heads(i).fire()||dequeued(head+i.U)
    // transition from deq to not deq - there should be exactly one
    // TODO: maybe do this in a smarter way so the compiler knows this and can optimize?
    when(previous_deq && !current_deq){
      // TODO: something other than % for wrap?
      head_next := (head+i.U)%numEntries.U
      for(j <- 0 until i){
        dequeued(head+j.U) := false.B
      }
    }
    previous_deq = previous_deq && current_deq
  }

  // Pipeline flushs - at bottom
  when(io.flush)
  {
    head_next := 0.U
    tail_next := 0.U
    valids.foreach(_ := false.B)
    dequeued.foreach(_ := false.B)
  }

  // Update for next CC
  head := head_next
  tail := tail_next

  assert(dequeued.zip(valids).map{ case (d, v) => !(d&&v)}.reduce(_&&_), "can be either dequeued or valid")

  if (O3PIPEVIEW_PRINTF) {
    when(io.tsc_reg>=O3_START_CYCLE.U) {
      for (i <- 0 until coreWidth) {
        when(io.enq_uops(i).valid) {
          printf("%d; O3PipeView:" + qName + ": %d\n",
            io.enq_uops(i).bits.debug_events.fetch_seq,
            io.tsc_reg)
        }
      }
    }
  }

  dontTouch(io)
  dontTouch(q_uop)
  dontTouch(valids)
  dontTouch(head)
  dontTouch(tail)
  dontTouch(head_next)
  dontTouch(tail_next)
}
