//******************************************************************************
// Copyright (c) 2012 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// BOOM Instruction Dispatcher
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


package boom.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common.{IQT_MFP, MicroOp, O3PIPEVIEW_PRINTF, uopLD, _}
import boom.util._
import chisel3.internal.naming.chiselName

class DispatchIO(implicit p: Parameters) extends BoomBundle
{
  // incoming microops from rename2
  val ren_uops = Vec(coreWidth, Flipped(DecoupledIO(new MicroOp)))

  // outgoing microops to issue queues
  // N issues each accept up to dispatchWidth uops
  // dispatchWidth may vary between issue queues
  val dis_uops = MixedVec(issueParams.map(ip=>Vec(ip.dispatchWidth, DecoupledIO(new MicroOp))))
  // io for busy table - used only for LSC
  val slice_busy_req_uops = if(boomParams.loadSliceMode) Some(Output(Vec(boomParams.loadSliceCore.get.dispatches, new MicroOp))) else None //TODO: change width
  val slice_busy_resps = if(boomParams.loadSliceMode) Some(Input(Vec(boomParams.loadSliceCore.get.dispatches, new BusyResp))) else None
  val slice_fp_busy_req_uops = if(boomParams.loadSliceMode && usingFPU) Some(Output(Vec(boomParams.loadSliceCore.get.dispatches, new MicroOp))) else None
  val slice_fp_busy_resps = if(boomParams.loadSliceMode && usingFPU) Some(Input(Vec(boomParams.loadSliceCore.get.dispatches, new BusyResp))) else None
  // brinfo & flush for LSC
  val slice_brinfo = if(boomParams.loadSliceMode) Some(Input(new BrResolutionInfo())) else None
  val slice_flush = if(boomParams.loadSliceMode) Some(Input(Bool())) else None

  val tsc_reg = Input(UInt(width=xLen.W))

  val lsc_perf = if(boomParams.loadSliceMode) Some(Output(new LscDispatchPerfCounters)) else None
}

/**
  *
  * Performance counters for LSC
  */
class LscDispatchPerfCounters(implicit p: Parameters) extends BoomBundle {
  val aq = Vec(decodeWidth, Bool()) // Number of insts in A-Q
  val bq = Vec(decodeWidth, Bool()) // Number of insts on B-Q
}

abstract class Dispatcher(implicit p: Parameters) extends BoomModule
{
  val io = IO(new DispatchIO)
}

/**
 * This Dispatcher assumes worst case, all dispatched uops go to 1 issue queue
 * This is equivalent to BOOMv2 behavior
 */
class BasicDispatcher(implicit p: Parameters) extends Dispatcher
{
  issueParams.map(ip=>require(ip.dispatchWidth == coreWidth))

  val ren_readys = io.dis_uops.map(d=>VecInit(d.map(_.ready)).asUInt).reduce(_&_)

  for (w <- 0 until coreWidth) {
    io.ren_uops(w).ready := ren_readys(w)
  }

  for {i <- 0 until issueParams.size
       w <- 0 until coreWidth} {
    val issueParam = issueParams(i)
    val dis        = io.dis_uops(i)

    dis(w).valid := io.ren_uops(w).valid && ((io.ren_uops(w).bits.iq_type & issueParam.iqType.U) =/= 0.U)
    dis(w).bits  := io.ren_uops(w).bits
  }
}
/**
 * LSC Dispatcher - contains both A and B queues and accesses the busy table at their end.
 */
class SliceDispatcher(implicit p: Parameters) extends Dispatcher {
  // Todo: These can probably be removed now. But we need the check somewhere else

  //issue requirements
  require(issueParams(0).iqType == IQT_INT.litValue()) // INT
  require(issueParams(1).iqType == IQT_MEM.litValue()) // MEM

  require(issueParams(0).dispatchWidth == 2)
  require(issueParams(1).dispatchWidth == 2)

  if (usingFPU) {
    require(issueParams(2).iqType == IQT_FP.litValue()) // FP
    require(issueParams(2).dispatchWidth == 1)
  }

  //slice queues
  val a_queue = Module(new SliceDispatchQueue(
    numEntries = boomParams.loadSliceCore.get.numAqEntries,
    qName = "a_queue",
    deqWidth = boomParams.loadSliceCore.get.aDispatches))
  val b_queue = Module(new SliceDispatchQueue(
    numEntries = boomParams.loadSliceCore.get.numBqEntries,
    qName = "b_queue",
    deqWidth = boomParams.loadSliceCore.get.bDispatches))
  a_queue.io.flush := io.slice_flush.get
  b_queue.io.flush := io.slice_flush.get
  a_queue.io.brinfo := io.slice_brinfo.get
  b_queue.io.brinfo := io.slice_brinfo.get
  a_queue.io.tsc_reg := io.tsc_reg
  b_queue.io.tsc_reg := io.tsc_reg

  val a_heads = WireInit(VecInit(a_queue.io.heads.map(_.bits)))
  val b_heads = WireInit(VecInit(b_queue.io.heads.map(_.bits)))

  //TODO: make this more fine-grained
  val a_ready = a_queue.io.enq_uops.map(_.ready).reduce(_ && _)
  val b_ready = b_queue.io.enq_uops.map(_.ready).reduce(_ && _)


  val queues_ready = a_ready && b_ready
  for (w <- 0 until coreWidth) {
    // TODO: check if it is possible to use only some of the ren_uops
    // only accept uops from rename if both queues are ready
    io.ren_uops(w).ready := queues_ready
    val uop = io.ren_uops(w).bits
    // check if b queue can actually process insn
    // TODO: Analyse: Is it necessary to add a guard protecting agains branches on B-Q? (In case of aliasing in IST)
    val can_use_b_alu = uop.fu_code_is(FUConstants.FU_ALU | FUConstants.FU_MUL | FUConstants.FU_DIV | (FUConstants.FU_BRU)) && !uop.is_br_or_jmp
    val use_b_queue = (uop.uopc === uopLD) || uop.uopc === uopSTA || (uop.is_lsc_b && can_use_b_alu)
    val use_a_queue = (uop.uopc =/= uopLD) && (!uop.is_lsc_b || !can_use_b_alu)

    // enqueue logic
    a_queue.io.enq_uops(w).valid := io.ren_uops(w).fire() && use_a_queue
    b_queue.io.enq_uops(w).valid := io.ren_uops(w).fire() && use_b_queue

    val uop_a = WireInit(uop)
    val uop_b = WireInit(uop)

    assert(!io.ren_uops(w).fire() || (a_queue.io.enq_uops(w).fire() || b_queue.io.enq_uops(w).fire()), "op from rename was swallowed")

    when(uop.uopc === uopSTA) {
      // In case of splitting stores. We need to put data generation in A (uopSTD) and
      //  address generation (uopSTA) in B. We X out the opposite lrs by assigning RT_X to the rtype
      uop_a.uopc := uopSTD
      uop_a.lrs1_rtype := RT_X

      uop_b.uopc := uopSTA
      uop_b.lrs2_rtype := RT_X
      // fsw and fsd fix - they need to go to a -> fp and b -> mem
      when(uop.iq_type === IQT_MFP) {
        uop_b.iq_type := IQT_MEM
        uop_b.lrs2_rtype := RT_X
        uop_a.iq_type := IQT_FP
        uop_a.uopc := uopSTA
        uop_a.lrs1_rtype := RT_X
      }
    }
    a_queue.io.enq_uops(w).bits := uop_a
    b_queue.io.enq_uops(w).bits := uop_b

    assert(!(io.ren_uops(w).fire() && use_b_queue && uop_b.is_br_or_jmp), "[Dispatcher] We are puttig a branch/jump on B-Q")

    // Perf counters
    io.lsc_perf.get.aq(w) := io.ren_uops(w).fire() && use_a_queue
    io.lsc_perf.get.bq(w) := io.ren_uops(w).fire() && use_b_queue && uop.uopc =/= uopSTA
  }

  // annotate heads with busy information
  for ((uop, idx) <- (a_heads ++ b_heads).zipWithIndex) {
    io.slice_busy_req_uops.get(idx) := uop
    val busy_resp = io.slice_busy_resps.get(idx)

    uop.prs1_busy := uop.lrs1_rtype === RT_FIX && busy_resp.prs1_busy
    uop.prs2_busy := uop.lrs2_rtype === RT_FIX && busy_resp.prs2_busy
    uop.prs3_busy := false.B

    //    assert(!(a_valid && busy_resp.prs1_busy && uop.lrs1_rtype === RT_FIX & uop.lrs1 === 0.U), "[rename] x0 is busy??")
    //    assert(!(a_valid && busy_resp.prs2_busy && uop.lrs2_rtype === RT_FIX & uop.lrs2 === 0.U), "[rename] x0 is busy??")
    if (usingFPU) {
      io.slice_fp_busy_req_uops.get(idx) := uop
      val flt_busy_resp = io.slice_fp_busy_resps.get(idx)
      // fp busy info
      when(uop.lrs1_rtype === RT_FLT) {
        uop.prs1_busy := flt_busy_resp.prs1_busy
      }
      when(uop.lrs2_rtype === RT_FLT) {
        uop.prs2_busy := flt_busy_resp.prs2_busy
      }
      uop.prs3_busy := uop.frs3_en && flt_busy_resp.prs3_busy
    }
  }

  // dispatch nothing by default
  for {dis_c <- io.dis_uops
       dis <- dis_c} {
    dis.valid := false.B
    dis.bits := DontCare
  }

  if(boomParams.unifiedIssueQueue){
//    io.dis_uops(LSC_DIS_MEM_PORT_IDX) := DontCare
//    io.dis_uops(3) := DontCare // TODO: clean up
//    if (usingFPU) {
//      io.dis_uops(LSC_DIS_FP_PORT_IDX) := DontCare
//    }
    var dis_idx = 0
    for(i <- 0 until boomParams.loadSliceCore.get.aDispatches){
      val dis = io.dis_uops(LSC_DIS_COMB_PORT_IDX)(dis_idx)
      dis.valid := a_queue.io.heads(i).valid
      a_queue.io.heads(i).ready := dis.ready
      dis.bits := a_heads(i)
      dis_idx += 1
    }
    for(i <- 0 until boomParams.loadSliceCore.get.bDispatches){
      val dis = io.dis_uops(LSC_DIS_COMB_PORT_IDX)(dis_idx)
      dis.valid := b_queue.io.heads(i).valid
      b_queue.io.heads(i).ready := dis.ready
      dis.bits := b_heads(i)
      dis_idx += 1
    }
    require(dis_idx == boomParams.loadSliceCore.get.dispatches)

  } else {
    require(boomParams.loadSliceCore.get.aDispatches == 1)
    require(boomParams.loadSliceCore.get.bDispatches == 1)

    // for now I'm just setting both dispatch ports to the same port - should work since they are never written to at the same time...
    val a_int_dispatch = io.dis_uops(LSC_DIS_INT_PORT_IDX)(LSC_DIS_A_PORT_IDX)
    val a_mem_dispatch = io.dis_uops(LSC_DIS_MEM_PORT_IDX)(LSC_DIS_A_PORT_IDX)
    val a_fp_dispatch = if (usingFPU) Some(io.dis_uops(LSC_DIS_FP_PORT_IDX)(LSC_DIS_A_PORT_IDX)) else None
    val b_int_dispatch = io.dis_uops(LSC_DIS_INT_PORT_IDX)(LSC_DIS_B_PORT_IDX)
    val b_mem_dispatch = io.dis_uops(LSC_DIS_MEM_PORT_IDX)(LSC_DIS_B_PORT_IDX)


    val a_issue_blocked = !a_mem_dispatch.ready || !a_int_dispatch.ready || a_fp_dispatch.map(!_.ready).getOrElse(false.B) // something from a is in a issue slot
    val b_issue_blocked = !b_mem_dispatch.ready || !b_int_dispatch.ready

    val a_head = a_heads(0)
    val b_head = b_heads(0)
    val a_valid = a_queue.io.heads(0).valid
    val b_valid = b_queue.io.heads(0).valid
    val a_deq = WireInit(false.B)
    a_queue.io.heads(0).ready := a_deq
    val b_deq = WireInit(false.B)
    b_queue.io.heads(0).ready := b_deq
    val a_head_mem = (a_head.iq_type & IQT_MEM) =/= 0.U
    val a_head_fp = (a_head.iq_type & IQT_FP) =/= 0.U
    val a_head_int = (a_head.iq_type & IQT_INT) =/= 0.U
    val b_head_mem = b_head.iq_type === IQT_MEM

    // this is handling the ready valid interface stricter than necessary to prevent errors
    // dispatch valid implies dispatch ready
    assert(!a_int_dispatch.valid || a_int_dispatch.ready)
    assert(!b_int_dispatch.valid || b_int_dispatch.ready)
    assert(!a_mem_dispatch.valid || a_mem_dispatch.ready)
    assert(!b_mem_dispatch.valid || b_mem_dispatch.ready)
    if (usingFPU) {
      assert(!a_fp_dispatch.get.valid || a_fp_dispatch.get.ready)
    }

    // dispatch implies dequeue
    assert(!a_int_dispatch.valid || a_deq)
    assert(!b_int_dispatch.valid || b_deq)
    assert(!a_mem_dispatch.valid || a_deq)
    assert(!b_mem_dispatch.valid || b_deq)
    if (usingFPU) {
      assert(!a_fp_dispatch.get.valid || a_deq)
    }

    // dequeue implies dispatch
    assert(!a_deq || (a_int_dispatch.valid || a_mem_dispatch.valid || a_fp_dispatch.map(_.valid).getOrElse(false.B)))
    assert(!b_deq || (b_int_dispatch.valid || b_mem_dispatch.valid))

    a_mem_dispatch.valid := false.B
    a_int_dispatch.valid := false.B
    b_mem_dispatch.valid := false.B
    b_int_dispatch.valid := false.B
    if (usingFPU) {
      a_fp_dispatch.get.valid := false.B
    }

    a_mem_dispatch.bits := a_head
    a_int_dispatch.bits := a_head
    b_mem_dispatch.bits := b_head
    b_int_dispatch.bits := b_head

    if (usingFPU) {
      a_fp_dispatch.get.bits := a_head
    }

    // put uops into issue queues
    when(a_valid && !a_issue_blocked) {
      a_deq := true.B
      when(a_head_mem) {
        a_mem_dispatch.valid := true.B
      }
      when(a_head_fp) {
        if (usingFPU) {
          a_fp_dispatch.get.valid := true.B
        }
      }
      when(a_head_int) {
        a_int_dispatch.valid := true.B
      }
    }
    when(b_valid && !b_issue_blocked) {
      b_deq := true.B
      when(b_head_mem) {
        b_mem_dispatch.valid := true.B
      }.otherwise {
        b_int_dispatch.valid := true.B
      }
    }
  }

  if(O3PIPEVIEW_PRINTF){ // dispatch is here because it does not happen driectly after rename anymore
    for(i <- 0 until boomParams.loadSliceCore.get.aDispatches){
      when (a_queue.io.heads(i).fire()) {
        printf("%d; O3PipeView:dispatch: %d\n", a_queue.io.heads(i).bits.debug_events.fetch_seq, io.tsc_reg)
      }
    }
    for(i <- 0 until boomParams.loadSliceCore.get.bDispatches){
      when (b_queue.io.heads(i).fire()) {
        printf("%d; O3PipeView:dispatch: %d\n", b_queue.io.heads(i).bits.debug_events.fetch_seq, io.tsc_reg)
      }
    }
  }
}

/**
 *  Tries to dispatch as many uops as it can to issue queues,
 *  which may accept fewer than coreWidth per cycle.
 *  When dispatchWidth == coreWidth, its behavior differs
 *  from the BasicDispatcher in that it will only stall dispatch when
 *  an issue queue required by a uop is full.
 */
class CompactingDispatcher(implicit p: Parameters) extends Dispatcher
{
  issueParams.map(ip => require(ip.dispatchWidth >= ip.issueWidth))

  val ren_readys = Wire(Vec(issueParams.size, Vec(coreWidth, Bool())))

  for (((ip, dis), rdy) <- issueParams zip io.dis_uops zip ren_readys) {
    val ren = Wire(Vec(coreWidth, Decoupled(new MicroOp)))
    ren <> io.ren_uops

    val uses_iq = ren map (u => (u.bits.iq_type & ip.iqType.U).orR)

    // Only request an issue slot if the uop needs to enter that queue.
    (ren zip io.ren_uops zip uses_iq) foreach {case ((u,v),q) =>
      u.valid := v.valid && q}

    val compactor = Module(new Compactor(coreWidth, ip.dispatchWidth, new MicroOp))
    compactor.io.in  <> ren
    dis <> compactor.io.out

    // The queue is considered ready if the uop doesn't use it.
    rdy := ren zip uses_iq map {case (u,q) => u.ready || !q}
  }

  (ren_readys.reduce((r,i) =>
      VecInit(r zip i map {case (r,i) =>
        r && i})) zip io.ren_uops) foreach {case (r,u) =>
          u.ready := r}
}

@chiselName
class SliceDispatchQueue(
                        val numEntries: Int = 8,
                        val qName: String,
                        val deqWidth: Int = 1
                        )(implicit p: Parameters) extends BoomModule
{
  val io = IO(new Bundle {
    val enq_uops = Vec(coreWidth, Flipped(DecoupledIO(new MicroOp)))
    val heads = Vec(deqWidth, DecoupledIO(new MicroOp()))

    val brinfo = Input(new BrResolutionInfo())
    val flush = Input(new Bool)
    val tsc_reg = Input(UInt(width=xLen.W)) // needed for pipeview
  })

  val qAddrSz: Int = log2Ceil(numEntries)

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

  // Queue state
  val q_uop = Reg(Vec(numEntries, new MicroOp()))
  val valids = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
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

  // Handle dequeues
  // on more so we also do something if all are dequeued
  for (i <- 0 until deqWidth+1) {
    val previous_deq = if(i==0) true.B else io.heads(i-1).fire()
    val current_deq =  if(i==deqWidth) false.B else io.heads(i).fire()
    assert(!(!previous_deq && current_deq), "deq only possible in order!")
    // transition from deq to not deq - there should be exactly one
    // TODO: maybe do this in a smarter way so the compiler knows this and can optimize?
    when(previous_deq && !current_deq){
      // TODO: something other than % for wrap?
      head_next := (head+i.U)%numEntries.U
    }
  }


  // Handle branch resolution
  //  On mispredict, find oldest that is killed and kill everyone younger than that
  //  On resolved. Update all branch masks in paralell. Updates also invalid entries, for simplicity.
  val updated_brmask = WireInit(false.B)//VecInit(Seq.fill(numEntries)(false.B))) //This wire decides if we should block the deque from head because of a branch resolution
  val entry_killed = WireInit(VecInit(Seq.fill(numEntries)(false.B)))
  when(io.brinfo.valid) {
    for (i <- 0 until numEntries) {
      val br_mask = q_uop(i).br_mask
      val entry_match = valids(i) && maskMatch(io.brinfo.mask, br_mask)

      when (entry_match && io.brinfo.mispredict) { // Mispredict
        entry_killed(i) := true.B
        valids(i) := false.B
      }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
        q_uop(i).br_mask := (br_mask & ~io.brinfo.mask)
        updated_brmask := true.B
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


  // Pipeline flushs
  when(io.flush)
  {
    head_next := 0.U
    tail_next := 0.U
    valids.map(_ := false.B)
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
    }
  }
  }

  for (w <- 0 until coreWidth)
  {
    io.enq_uops(w).ready := !valids(tail+w.U) //TODO: ensure it is possible to take only some from rename
  }

  // Update for next CC
  head := head_next
  tail := tail_next

  // TODO: update
//  // dequeue implies queue valid (not empty)
//  assert(!io.deq_uop || !empty)
//
//  // empty implies ready
//  assert(!empty || ready)
//
//  // enqueue implies ready
//  assert(!io.enq_uops.map(_.valid).reduce(_||_) || ready)
//
//  // No deques on flush
//  assert(!(io.deq_uop && io.flush))

  if (O3PIPEVIEW_PRINTF) {
    for (i <- 0 until coreWidth) {
      when (io.enq_uops(i).valid) {
        printf("%d; O3PipeView:"+qName+": %d\n",
          io.enq_uops(i).bits.debug_events.fetch_seq,
          io.tsc_reg)
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
