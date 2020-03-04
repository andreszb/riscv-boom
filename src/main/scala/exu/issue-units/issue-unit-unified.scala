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
import chisel3._
import chisel3.internal.naming.chiselName
import freechips.rocketchip.config.Parameters

/**
  * Specific type of issue unit
  *
  * @param params issue queue params
  * @param numWakeupPorts number of wakeup ports for the issue queue
  */
class IssueUnitUnified(
                           params: IssueParams,
                           numWakeupPorts: Int)
                      (implicit p: Parameters)
  extends IssueUnit(params.numEntries, params.issueWidth, numWakeupPorts, params.iqType, params.dispatchWidth)
{
  require(params.dispatchWidth == numIssueSlots, "one slot per dispatch port!")
  require(params.dispatchWidth == boomParams.loadSliceCore.get.dispatches())

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).in_uop.valid := false.B
    issue_slots(i).in_uop.bits  := dis_uops(i)
    when (io.dis_uops(i).fire()) {
      issue_slots(i).in_uop.valid := true.B
    }
    // TODO: figure out if this is necessary or if options are bulk connected
    for ((s, p) <- issue_slots(i).wakeup_ports zip io.wakeup_ports){
      s.bits.reg_type.get := p.bits.reg_type.get
    }
    issue_slots(i).wakeup_ports := io.wakeup_ports
    issue_slots(i).ldspec_dst   := io.spec_ld_wakeup
    issue_slots(i).ldspec_miss  := io.ld_miss
    issue_slots(i).brinfo       := io.brinfo
    issue_slots(i).kill         := io.flush_pipeline
    issue_slots(i).clear        := false.B
  }

  // TODO: make sure that insns are actually executed in the right order
  var aIndices: List[Int] = Nil
  var bIndices: List[Int] = Nil
  var idx = 0
  for(i <- 0 until boomParams.loadSliceCore.get.aDispatches){
    aIndices = idx :: aIndices
    idx += 1
  }

  for(i <- 0 until boomParams.loadSliceCore.get.bDispatches){
    bIndices = idx :: bIndices
    idx += 1
  }

  val readys = issue_slots.map(!_.will_be_valid)

  val a_ready = aIndices.map(readys(_)).reduce(_ && _)
  val b_ready = bIndices.map(readys(_)).reduce(_ && _)
  //-------------------------------------------------------------
  // Dispatch/Entry Logic
  // did we find a spot to slide the new dispatched uops into?

//  for (w <- 0 until dispatchWidth) {
//    io.dis_uops(w).ready := readys(w)
//  }

  aIndices.foreach(io.dis_uops(_).ready := a_ready)
  bIndices.foreach(io.dis_uops(_).ready := b_ready)

  //-------------------------------------------------------------
  // Issue Select Logic

  // set default
  for (w <- 0 until issueWidth) {
    io.iss_valids(w) := false.B
    io.iss_uops(w)   := NullMicroOp
    // unsure if this is overkill
    io.iss_uops(w).prs1 := 0.U
    io.iss_uops(w).prs2 := 0.U
    io.iss_uops(w).prs3 := 0.U
    io.iss_uops(w).lrs1_rtype := RT_X
    io.iss_uops(w).lrs2_rtype := RT_X
  }

  val requests = issue_slots.map(s => s.request)
  val port_issued = Array.fill(issueWidth){Bool()}
  for (w <- 0 until issueWidth) {
    port_issued(w) = false.B
  }

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).grant := false.B
    var uop_issued = false.B

    for (w <- 0 until issueWidth) {
      val iq_type_match = (issue_slots(i).uop.iq_type === io.iq_types.get(w))
      val fu_type_match = ((issue_slots(i).uop.fu_code & io.fu_types(w)) =/= 0.U)
      iq_type_match.suggestName(s"iq_type_match_slot_${i}_exu_$w")
      fu_type_match.suggestName(s"fu_type_match_slot_${i}_exu_$w")
      val can_allocate = fu_type_match && iq_type_match

      when (requests(i) && !uop_issued && can_allocate && !port_issued(w)) {
        issue_slots(i).grant := true.B
        io.iss_valids(w) := true.B
        io.iss_uops(w) := issue_slots(i).uop
      }
      val was_port_issued_yet = port_issued(w)
      port_issued(w) = (requests(i) && !uop_issued && can_allocate) | port_issued(w)
      uop_issued = (requests(i) && can_allocate && !was_port_issued_yet) | uop_issued
    }
  }
}
