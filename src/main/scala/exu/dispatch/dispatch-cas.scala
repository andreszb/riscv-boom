
package boom.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common.{IQT_MFP, MicroOp, O3PIPEVIEW_PRINTF, uopLD, _}
import boom.util._
import chisel3.internal.naming.chiselName

class CasDispatcher(implicit p: Parameters) extends Dispatcher {
  val casParams = boomParams.casParams.get

  val inq = Module(new SramDispatchQueueCompactingShifting( DispatchQueueParams(
    numEntries = casParams.numInqEntries,
    qName="INQ",
    deqWidth=casParams.inqDispatches,
    enqWidth= casParams.slidingOffset)))

  val sq = Module(new SramDispatchQueueCompactingShifting( DispatchQueueParams(
    numEntries = casParams.numSqEntries,
    qName="SQ",
    deqWidth=casParams.windowSize,
    enqWidth=coreWidth)))

  val sq_issue_grant = Wire(Vec(casParams.windowSize, Bool()))
  val sq_enqueue_inq = Wire(Vec(casParams.slidingOffset, Bool()))

  // We dont use the dispatch port. Everything goes via the heads ports
  io.dis_uops.map(_ := DontCare)
  io.dis_uops.map(_.map(_.valid := false.B))

  // Handle enqueing of renamed uops. All just go straight to the Speculative Queue SQ
  for (i <- 0 until coreWidth) {
    sq.io.enq_uops(i) <> io.ren_uops(i)
  }

  // Busy-lookup


  val sq_heads = Wire(Vec(casParams.windowSize, new MicroOp()))
  val sq_heads_ready = WireInit(VecInit(Seq.fill(casParams.windowSize)(false.B)))
  val sq_heads_valid = WireInit(VecInit(Seq.fill(casParams.windowSize)(false.B)))
  (sq_heads zip sq.io.heads).map{case (l,r) => l := r.bits}
  (sq_heads_valid zip sq.io.heads).map{case (l,r) => l := r.valid}

  val inq_heads = Wire(Vec(casParams.inqDispatches, new MicroOp()))
  val inq_heads_ready = WireInit(VecInit(Seq.fill(casParams.inqDispatches)(false.B)))
  val inq_heads_valid = WireInit(VecInit(Seq.fill(casParams.inqDispatches)(false.B)))
  (inq_heads zip inq.io.heads).map{case (l,r) => l := r.bits}
  (inq_heads_valid zip inq.io.heads).map{case (l,r) => l := r.valid}

  val heads = sq_heads ++ inq_heads
  val heads_ready = sq_heads_ready ++ inq_heads_ready

  val num_heads = casParams.windowSize + casParams.inqDispatches

  for (i <- 0 until num_heads) {

    io.busy_req_uops.get(i) := heads(i)
    io.fp_busy_req_uops.get(i) := heads(i)

    val rs1_busy = WireInit(true.B)
    val rs2_busy = WireInit(true.B)
    val rs3_busy = WireInit(true.B)

    when(heads(i).lrs1_rtype === RT_FIX) {
      rs1_busy := io.busy_resps.get(i).prs1_busy
    }.elsewhen(heads(i).lrs1_rtype === RT_FLT) {
      rs1_busy := io.fp_busy_resps.get(i).prs1_busy
    }.otherwise {
      rs1_busy := false.B
    }

    when(heads(i).lrs2_rtype === RT_FIX) {
      rs2_busy := io.busy_resps.get(i).prs2_busy
    }.elsewhen(heads(i).lrs2_rtype === RT_FLT) {
      rs2_busy := io.fp_busy_resps.get(i).prs2_busy
    }.otherwise {
      rs2_busy := false.B
    }

    rs3_busy := heads(i).frs3_en && io.fp_busy_resps.get(i).prs3_busy

    heads(i).prs1_busy := rs1_busy
    heads(i).prs2_busy := rs2_busy
    heads(i).prs3_busy := rs3_busy
    heads_ready(i) := !rs1_busy && !rs2_busy && !rs3_busy
  }


  // Pass out the heads to the UIQ. Dont pass the ready signal to the SQ yet
  // It will be or'ed with the dequeue from Inq
  for (i <- 0 until casParams.windowSize) {
    io.sq_heads.get(i).valid := sq_heads_valid(i) && sq_heads_ready(i)
    io.sq_heads.get(i).bits := sq_heads(i)
    sq_issue_grant(i) := io.sq_heads.get(i).ready
  }

  for (i <- 0 until casParams.inqDispatches) {
    io.inq_heads.get(i).valid := inq_heads_valid(i) && inq_heads_ready(i)
    io.inq_heads.get(i).bits := inq_heads(i)
    inq.io.heads(i).ready := io.inq_heads.get(i).ready
  }

  // SQ > INQ connection

  for (i <- 0 until casParams.slidingOffset) {
    inq.io.enq_uops(i).bits := sq_heads(i)
    inq.io.enq_uops(i).valid := sq_heads_valid(i) && !sq_heads_ready(i)
    sq_enqueue_inq(i) := sq_heads_valid(i) && !sq_heads_ready(i) &&  inq.io.enq_uops(i).ready
  }

  // Connect ready signal to SQ
  for (i <- 0 until casParams.slidingOffset) {
    sq.io.heads(i).ready := sq_enqueue_inq(i) || sq_issue_grant(i)
    assert(! (sq_enqueue_inq(i) && sq_issue_grant(i)), "[cas-dis] Issue and enqueue SQ head at the same time")
  }

  for (i <- casParams.slidingOffset until casParams.windowSize) {
    sq.io.heads(i).ready := sq_issue_grant(i)
  }


  // Branch resolution and flushes
  // Route brinfo and flush into the fifos
  inq.io.brinfo := io.brinfo.get
  inq.io.flush := io.flush.get
  sq.io.brinfo := io.brinfo.get
  sq.io.flush  := io.flush.get

  // Route in tsc for pipeview
  inq.io.tsc_reg := io.tsc_reg
  sq.io.tsc_reg := io.tsc_reg

  // Performance counteres
  (io.cas_perf.get.sq_dis zip io.sq_heads.get).map{case(l,r) => l := r.fire()}
  (io.cas_perf.get.inq_dis zip io.inq_heads.get).map{case(l,r) => l := r.fire()}

}