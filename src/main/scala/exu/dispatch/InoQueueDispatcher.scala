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

import boom.common._
import chisel3._
import chisel3.internal.naming.chiselName
import freechips.rocketchip.config.Parameters

@chiselName
class InoQueueDispatcher(implicit p: Parameters) extends Dispatcher {

  val inq = Module(boomParams.inoParams.get.queueTypes match {
    case QUEUE_NAIVE => new NaiveDispatchQueueCompactingShifting(boomParams.inoParams.get.queueParams)
    case QUEUE_SINGLE => new LayeredDispatchQueueCompactingShifting(boomParams.inoParams.get.queueParams, multi = false)
    case QUEUE_MULTI => new LayeredDispatchQueueCompactingShifting(boomParams.inoParams.get.queueParams, multi = true)
  })

  // We dont use the dispatch port. Everything goes via the heads ports
  io.dis_uops.map(_ := DontCare)
  io.dis_uops.map(_.map(_.valid := false.B))

  // Handle enqueing of renamed uops. All just go straight to the Ino Queue
  for (i <- 0 until coreWidth) {
    inq.io.enq_uops(i) <> io.ren_uops(i)
  }

  val inq_heads = Wire(Vec(coreWidth, new MicroOp()))
  val inq_heads_ready = WireInit(VecInit(Seq.fill(coreWidth)(false.B)))
  val inq_heads_valid = WireInit(VecInit(Seq.fill(coreWidth)(false.B)))
  (inq_heads zip inq.io.heads).map{case (l,r) => l := r.bits}
  (inq_heads_valid zip inq.io.heads).map{case (l,r) => l := r.valid}

  val heads = inq_heads
  val heads_ready = inq_heads_ready

  val num_heads = coreWidth

  val load_spec_dsts = io.spec_ld_wakeup.get.map(w => RegNext(w.bits))
  val load_spec_valids = io.spec_ld_wakeup.get.map(w => RegNext(w.valid) && !io.ld_miss.get)
  for (i <- 0 until num_heads) {

    io.busy_req_uops.get(i) := heads(i)
    io.fp_busy_req_uops.get(i) := heads(i)

    val rs1_busy = WireInit(true.B)
    val rs2_busy = WireInit(true.B)
    val rs3_busy = WireInit(true.B)

    when(heads(i).lrs1_rtype === RT_FIX) {
      rs1_busy := io.busy_resps.get(i).prs1_busy

      for((ld, lv) <- load_spec_dsts zip load_spec_valids) {
        when(lv && heads(i).prs1 === ld) {
          rs1_busy := false.B
        }
      }
    }.elsewhen(heads(i).lrs1_rtype === RT_FLT) {
      rs1_busy := io.fp_busy_resps.get(i).prs1_busy
    }.otherwise {
      rs1_busy := false.B
    }

    when(heads(i).lrs2_rtype === RT_FIX) {
      rs2_busy := io.busy_resps.get(i).prs2_busy

      for((ld, lv) <- load_spec_dsts zip load_spec_valids) {
        when(lv && heads(i).prs2 === ld) {
          rs2_busy := false.B
        }
      }
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

  for (i <- 0 until coreWidth) {
    io.q1_heads.get(i).valid := inq_heads_valid(i) && inq_heads_ready(i)
    io.q1_heads.get(i).bits := inq_heads(i)
    inq.io.heads(i).ready := io.q1_heads.get(i).ready
  }

  // Branch resolution and flushes
  // Route brinfo and flush into the fifos
  inq.io.brupdate := io.brupdate.get
  inq.io.flush := io.flush.get

  // Route in tsc for pipeview
  inq.io.tsc_reg := io.tsc_reg
}