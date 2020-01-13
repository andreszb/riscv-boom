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
import boom.common.{MicroOp, uopLD, _}
import boom.util._

class DispatchIO(implicit p: Parameters) extends BoomBundle
{
  // incoming microops from rename2
  val ren_uops = Vec(coreWidth, Flipped(DecoupledIO(new MicroOp)))

  // outgoing microops to issue queues
  // N issues each accept up to dispatchWidth uops
  // dispatchWidth may vary between issue queues
  val dis_uops = MixedVec(issueParams.map(ip=>Vec(ip.dispatchWidth, DecoupledIO(new MicroOp))))
  // io for busy table - used only for LSC
  val slice_busy_req_uops = if(boomParams.loadSliceMode) Some(Output(Vec(coreWidth, new MicroOp))) else None
  val slice_busy_resps = if(boomParams.loadSliceMode) Some(Input(Vec(coreWidth, new BusyResp))) else None
  // brinfo & flush for LSC
  val slice_brinfo = if(boomParams.loadSliceMode) Some(Input(new BrResolutionInfo())) else None
  val slice_flush = if(boomParams.loadSliceMode) Some(Input(Bool())) else None
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
class SliceDispatcher(implicit p: Parameters) extends Dispatcher
{
  // todo: don't hardcode issue order
  require(issueParams(0).iqType==IQT_INT.litValue()) // A
  require(issueParams(1).iqType==IQT_MEM.litValue()) // MEM
//  require(issueParams(2).iqType==IQT_INT.litValue()) // B

  issueParams.map(ip => require(ip.dispatchWidth == 1)) // for now we support only one instruction per queue to preserve in order
  val a_dispatch = io.dis_uops(0).head
  val mem_dispatch = io.dis_uops(1).head
//  val b_dispatch = io.dis_uops(2).head


  // state that remembers if instruction in MEM issue slot belongs to A or B queue
  val mem_issue_is_b = WireInit(false.B)

  val a_blocked_mem = !mem_issue_is_b && !mem_dispatch.ready
  val b_blocked_mem = mem_issue_is_b && !mem_dispatch.ready
  val a_blocked = a_blocked_mem || !a_dispatch.ready
  val b_blocked = b_blocked_mem //|| !b_dispatch.ready

  val a_queue = Module(new SliceDispatchQueue())
  val b_queue = Module(new SliceDispatchQueue())
  a_queue.io.flush := io.slice_flush.get
  b_queue.io.flush := io.slice_flush.get
  a_queue.io.brinfo := io.slice_brinfo.get
  b_queue.io.brinfo := io.slice_brinfo.get
  val a_head = WireInit(a_queue.io.head.bits)
  val b_head = WireInit(b_queue.io.head.bits)
  val a_valid = a_queue.io.head.valid
  val b_valid = b_queue.io.head.valid
  val a_head_mem = a_head.iq_type === IQT_MEM
  val b_head_mem = b_head.iq_type === IQT_MEM
  val a_ready = a_queue.io.enq_uops.map(_.ready).reduce(_ && _)
  val b_ready = b_queue.io.enq_uops.map(_.ready).reduce(_ && _)

  
  val queues_ready =  a_ready && b_ready
  for (w <- 0 until coreWidth) {
    // TODO: check if it is possible to use only some of the ren_uops
    // only accept uops from rename if both queues are ready
    io.ren_uops(w).ready := queues_ready
    val uop = io.ren_uops(w).bits
    // use b que if uop is load for now
    val use_b_queue = uop.uopc === uopLD
    // enqueue logic
    a_queue.io.enq_uops(w).valid := io.ren_uops(w).fire() && !use_b_queue
    b_queue.io.enq_uops(w).valid := io.ren_uops(w).fire() && use_b_queue
    a_queue.io.enq_uops(w).bits := uop
    b_queue.io.enq_uops(w).bits := uop
  }
  // dispatch nothing by default
  for {i <- 0 until issueParams.size
       w <- 0 until 1} {
    io.dis_uops(i)(w).valid := false.B
    io.dis_uops(i)(w).bits  := DontCare
  }

  // annotate heads with busy information
  io.slice_busy_req_uops.get(0) := a_head
  io.slice_busy_req_uops.get(1) := b_head
  val a_busy_resp = io.slice_busy_resps.get(0) 
  val b_busy_resp = io.slice_busy_resps.get(1) 

  a_head.prs1_busy := a_head.lrs1_rtype === RT_FIX && a_busy_resp.prs1_busy
  a_head.prs2_busy := a_head.lrs2_rtype === RT_FIX && a_busy_resp.prs2_busy
  a_head.prs3_busy := a_head.frs3_en && a_busy_resp.prs3_busy

  b_head.prs1_busy := b_head.lrs1_rtype === RT_FIX && b_busy_resp.prs1_busy
  b_head.prs2_busy := b_head.lrs2_rtype === RT_FIX && b_busy_resp.prs2_busy
  b_head.prs3_busy := b_head.frs3_en && b_busy_resp.prs3_busy

  assert(!(a_valid && a_busy_resp.prs1_busy && a_head.lrs1 === 0.U), "[rename] x0 is busy??")
  assert(!(a_valid && a_busy_resp.prs2_busy && a_head.lrs2 === 0.U), "[rename] x0 is busy??")
  assert(!(b_valid && b_busy_resp.prs1_busy && b_head.lrs1 === 0.U), "[rename] x0 is busy??")
  assert(!(b_valid && b_busy_resp.prs2_busy && b_head.lrs2 === 0.U), "[rename] x0 is busy??")
  
  // put uops into issue queues
  when((a_valid && a_head_mem && !a_blocked) && (b_valid && b_head_mem && !b_blocked)) {
    // both are mem and could be dispatched
    // figure out if a or b come first if we have two possible mem operations - take the one that was not last
    when(mem_issue_is_b){
      // issue from a
      mem_dispatch.bits := a_head
      mem_dispatch.valid := true.B
      mem_issue_is_b := false.B
    } .otherwise{
      // issue from b
      mem_dispatch.bits := b_head
      mem_dispatch.valid := true.B
      mem_issue_is_b := true.B
    }
  } .otherwise { // not two potential memory operations => can issue both
    when(a_valid && !a_blocked){
      a_queue.io.deq_uop := true.B
      when(a_head_mem){
        mem_dispatch.bits := a_head
        mem_dispatch.valid := true.B
        mem_issue_is_b := false.B
      } .otherwise{
        a_dispatch.bits := a_head
        a_dispatch.valid := true.B
      }
    }
    when(b_valid && !b_blocked){
      b_queue.io.deq_uop := true.B
      when(b_head_mem){
        mem_dispatch.bits := b_head
        mem_dispatch.valid := true.B
        mem_issue_is_b := true.B
      } .otherwise{
//        b_dispatch.bits := b_head
//        b_dispatch.valid := true.B
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

class SliceDispatchQueue(
                        val numEntries: Int = 8,
                        val qAddrSz: Int = 3
                        )(implicit p: Parameters) extends BoomModule
{
  val io = IO(new Bundle {
    val enq_uops = Vec(coreWidth, Flipped(DecoupledIO(new MicroOp)))
    val deq_uop = Input(Bool())
    val head = Valid(new MicroOp())

    val brinfo = Input(new BrResolutionInfo())
    val flush = Input(new Bool)
  })

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
  val head = RegInit(0.U(qAddrSz.W))
  val tail = RegInit(0.U(qAddrSz.W))
  val empty = RegInit(true.B)
  val ready = RegInit(true.B)


  // Wires for calculating state in next CC
  val head_next = Wire(UInt(qAddrSz.W))
  val tail_next = Wire(UInt(qAddrSz.W))
  val empty_next = Wire(Bool())
  val ready_next = Wire(Bool())

  // Handle enqueues
  //  Use WrapInc to sequentilize an arbitrary number of enqueues.
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
      q_uop(enq_idx(w)) := io.enq_uops(w).bits
    }
  }
  // Update tail
  //  We have already incremented the tail pointer in the previous loop.
  //  Only needs a last increment if the last enq port also fired
  tail_next := Mux(io.enq_uops(coreWidth - 1).fire, WrapInc(enq_idx(coreWidth - 1), numEntries), enq_idx(coreWidth - 1))

  // Handle dequeues
  //  Simply increment the head
  when(io.deq_uop)
  {
    head_next := WrapInc(head, numEntries)
  }


  // Handle branch resolution
  //  On mispredict, find oldest that is killed and kill everyone younger than that
  //  On resolved. Update all branch masks in paralell. Updates also invalid entries, for simplicity.
  when(io.brinfo.valid) {
    val is_valid =  WireInit(VecInit(Seq.fill(numEntries){true.B}))// Used to stop loop when we reach the head
    for (i <- 0 until numEntries) {
      val idx = wrapIndex(tail - i.U) // Calculate idx from tail->head
      if (i == 0) {
        is_valid(i) := !empty // Only case when the tail is invalid is if the queue is empty
      } else { // Else: we check if the previous entry was the head OR the previous entry was invalid
        is_valid(i) := is_valid(i - 1) && !(WrapInc(idx, numEntries) === head)
      }
      val br_mask = q_uop(idx).br_mask
      val entry_match = is_valid(i) && maskMatch(io.brinfo.mask, br_mask)

      when (entry_match && io.brinfo.mispredict) { // Mispredict
        tail_next := idx
      }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
        q_uop(idx).br_mask := (br_mask & ~io.brinfo.mask)
      }
    }
  }

// Pipeline flushs
  when(io.flush)
  {
    empty_next := true.B
    head_next := 0.U
    tail_next := 0.U
    ready_next := true.B
  }



  // Empty
  //  Little hack: If an element was dequeued it is currently impossible that the queue will be full
  when(head_next === tail_next && head_next =/= head) {
    empty := true.B
  }

  // Ready?
  // TODO: Make this smarter
  val check_capacity = Wire(Vec(coreWidth, UInt(qAddrSz.W)))
  ready_next := true.B
  for(w <- 0 until coreWidth)
  {
    if (w == 0) {
      check_capacity(w) := tail_next
    } else {
      check_capacity(w) := WrapInc(check_capacity(w-1), numEntries)
    }
    when(check_capacity(w) === head_next) {
      ready_next := false.B
    }
  }



  // Route out IO
  io.head.bits := q_uop(head)
  io.head.valid := !empty
  for (w <- 0 until coreWidth)
  {
    io.enq_uops(w).ready := ready
  }

  // Update for next CC
  head := head_next
  tail := tail_next
  ready := ready_next
  empty := empty_next

}
