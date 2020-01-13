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

import boom.common._
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
    val enq_uops = Input(Vec(coreWidth, Flipped(DecoupledIO(new MicroOp))))
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
  tail_next := Mux(io.enq_uops(coreWidth).fire, WrapInc(enq_idx(coreWidth), numEntries), enq_idx(coreWidth))

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
    val loop_cond = true.B // Used to stop loop when we reach the head
    for (i <- 0 until numEntries) {
      when(loop_cond) {
        val idx = wrapIndex(tail - i.U)
        when(idx === head) { // Head reached. Stop loop
          loop_cond := false.B
        }
        val br_mask = q_uop(idx).br_mask
        val entry_match = maskMatch(io.brinfo.mask, br_mask)

        when (entry_match && io.brinfo.mispredict) { // Mispredict
          tail_next := idx
        }.elsewhen(entry_match && !io.brinfo.mispredict) { // Resolved
          q_uop(idx).br_mask := (br_mask & ~io.brinfo.mask)
        }

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
  when(head_next === tail_next && head_next != head) {
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
