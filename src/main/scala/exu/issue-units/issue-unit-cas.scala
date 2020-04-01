//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import boom.common._
import boom.util.WrapInc
import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util.{PopCount, isPow2, log2Ceil}
import freechips.rocketchip.config.Parameters

/**
  * Specific type of issue unit
  *
  * @param params issue queue params
  * @param numWakeupPorts number of wakeup ports for the issue queue
  */
@chiselName
class IssueUnitCasUnified(
                           params: IssueParams,
                           numWakeupPorts: Int)
                         (implicit p: Parameters)
  extends IssueUnit(params.numEntries, params.issueWidth, numWakeupPorts, params.iqType, params.dispatchWidth)
{
  val casParams = boomParams.casParams.get

  def uop_ready(u: MicroOp): Bool = {
    val res = Wire(Bool())
    res := !u.prs1_busy && !u.prs2_busy && !u.prs3_busy
    res
  }

  // CASINO has no OoO-IQ
  require(params.numEntries ==0)
  require(params.dispatchWidth == 0)


  // Issue Select Logic
  for(w <- 0 until issueWidth) {
    io.iss_valids(w) := false.B
    io.iss_uops(w)   := NullMicroOp
    // unsure if this is overkill
    io.iss_uops(w).prs1 := 0.U
    io.iss_uops(w).prs2 := 0.U
    io.iss_uops(w).prs3 := 0.U
    io.iss_uops(w).lrs1_rtype := RT_X
    io.iss_uops(w).lrs2_rtype := RT_X
  }

  // FIFO heads "request" issue when they are valid and not-busy
  val requests = io.inq_heads.get.map(h => h.valid && uop_ready(h.bits)) ++
                 io.sq_heads.get.map(h => h.valid && uop_ready(h.bits))

  // Issue Unit Grants are connected to the ready signals => signalling dequeue
  val grants =  io.inq_heads.get.map(h => h.ready) ++
                io.sq_heads.get.map(h => h.ready)

  val candidate_uops = io.inq_heads.get.map(h => h.bits) ++ io.sq_heads.get.map(h => h.bits)
  val port_issued = Array.fill(issueWidth){false.B}


  val len = casParams.inqDispatches + casParams.windowSize

  for (i <- 0 until len) {
    grants(i) := false.B
    var uop_issued = false.B

    for (w <- 0 until issueWidth) {
      val iq_type_match = (candidate_uops(i).iq_type === io.iq_types.get(w))
      val fu_type_match = ((candidate_uops(i).fu_code & io.fu_types(w)) =/= 0.U)
      iq_type_match.suggestName(s"iq_type_match_slot_${i}_exu_$w")
      fu_type_match.suggestName(s"fu_type_match_slot_${i}_exu_$w")
      val can_allocate = fu_type_match && iq_type_match

      when (requests(i) && !uop_issued && can_allocate && !port_issued(w)) {
        grants(i) := true.B
        io.iss_valids(w) := true.B
        io.iss_uops(w) := candidate_uops(i)
      }
      val was_port_issued_yet = port_issued(w)
      port_issued(w) = (requests(i) && !uop_issued && can_allocate) | port_issued(w)
      uop_issued = (requests(i) && can_allocate && !was_port_issued_yet) | uop_issued
    }
  }
  assert(!(PopCount(grants) > issueWidth.U), "[iss-cas] More grants than issue ports")
}
