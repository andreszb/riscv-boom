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
import chisel3.util.PopCount
import freechips.rocketchip.config.Parameters

/**
  * Specific type of issue unit
  *
  * @param params issue queue params
  * @param numWakeupPorts number of wakeup ports for the issue queue
  */
class IssueUnitDnbUnified(
                           params: IssueParams,
                           numWakeupPorts: Int)
                           (implicit p: Parameters)
  extends IssueUnit(params.numEntries, params.issueWidth, numWakeupPorts, params.iqType, params.dispatchWidth)
{
  //TODO: figure out how to allocate some issue port explicitly fro DLQ and CRQ?
  //TODO: allow non-urgent dlq insns if fu is available?

  // rob_head_idx + dlqRobUrgentDist >= dlq_head.rob_idx
  val dlq_urgent = io.dlq_head.get.valid && (WrapInc(io.rob_head_idx.get, boomParams.dnbParams.get.dlqRobUrgentDist) >= io.dlq_head.get.bits.rob_idx)
  val dlq_grant = WireInit(false.B)
  val dlq_to_slot = WireInit(false.B)
  io.dlq_head.get.ready := dlq_grant || dlq_to_slot
  //-------------------------------------------------------------
  // Figure out how much to shift entries by

  // need to be able ot shift by one mor in order to incorporate dlq
  val maxShift = dispatchWidth+1
  // TODO: figure out if this might increase the critical path
  val vacants = issue_slots.map(s => !(s.valid)) ++ Seq(dlq_urgent && !dlq_grant) ++ io.dis_uops.map(_.valid).map(!_.asBool)
  val shamts_oh = Array.fill(numIssueSlots+maxShift) {Wire(UInt(width=maxShift.W))}
  // track how many to shift up this entry by by counting previous vacant spots
  def SaturatingCounterOH(count_oh:UInt, inc: Bool, max: Int): UInt = {
    val next = Wire(UInt(width=max.W))
    next := count_oh
    when (count_oh === 0.U && inc) {
      next := 1.U
    } .elsewhen (!count_oh(max-1) && inc) {
      next := (count_oh << 1.U)
    }
    next
  }
  shamts_oh(0) := 0.U
  for (i <- 1 until numIssueSlots + maxShift) {
    shamts_oh(i) := SaturatingCounterOH(shamts_oh(i-1), vacants(i-1), maxShift)
  }

  //-------------------------------------------------------------

  // which entries' uops will still be next cycle? (not being issued and vacated)
  val will_be_valid = (0 until numIssueSlots).map(i => issue_slots(i).will_be_valid) ++
    Seq(io.dlq_head.get.valid && !dlq_grant) ++
    (0 until dispatchWidth).map(i => io.dis_uops(i).valid &&
      !dis_uops(i).exception &&
      !dis_uops(i).is_fence &&
      !dis_uops(i).is_fencei)

  val queue_added = WireInit(VecInit(Seq.fill(maxShift)(false.B)))
  dontTouch(queue_added)
  val uops = issue_slots.map(s=>s.out_uop) ++
    Seq(io.dlq_head.get.bits) ++
    dis_uops.map(s=>s)
  for (i <- 0 until numIssueSlots) {
    issue_slots(i).in_uop.valid := false.B
    issue_slots(i).in_uop.bits  := uops(i+1)
    for (j <- 1 to maxShift by 1) {
      when (shamts_oh(i+j) === (1 << (j-1)).U) {
        issue_slots(i).in_uop.valid := will_be_valid(i+j)
        issue_slots(i).in_uop.bits  := uops(i+j)
        if(i+j >= numIssueSlots){
          queue_added(i+j-numIssueSlots) := true.B
        }
      }
    }
    issue_slots(i).wakeup_ports := io.wakeup_ports
    issue_slots(i).ldspec_dst   := io.spec_ld_wakeup
    issue_slots(i).ldspec_miss  := io.ld_miss
    issue_slots(i).brinfo       := io.brinfo
    issue_slots(i).kill         := io.flush_pipeline
    issue_slots(i).clear        := shamts_oh(i) =/= 0.U
  }

  //-------------------------------------------------------------
  // Dispatch/Entry Logic
  // did we find a spot to slide the new dispatched uops into?
  //TODO: DLQ
//  val will_be_available = (0 until numIssueSlots).map(i =>
//    (!issue_slots(i).will_be_valid || issue_slots(i).clear) && !(issue_slots(i).in_uop.valid))
//  val num_available = PopCount(will_be_available)
  dlq_to_slot := queue_added(0)
  for (w <- 0 until dispatchWidth) {
    // TODO: figure out if this might increase the critical path
    io.dis_uops(w).ready := queue_added(w+1) // RegNext(num_available > w.U)
  }
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

  val port_issued = Array.fill(issueWidth){false.B}
  // urgent_dlq > slots > crq
  val requests = Seq(dlq_urgent)++
    issue_slots.map(s => s.request)++
    Seq(io.crq_head.get.valid)
  val grants = Seq(dlq_grant)++
    issue_slots.map(s => s.grant)++
    Seq(io.crq_head.get.ready)
  val candidate_uops = Seq(io.dlq_head.get.bits)++
    issue_slots.map(s => s.uop)++
    Seq(io.crq_head.get.bits)

  require(requests.length == numIssueSlots+2)
  require(grants.length == numIssueSlots+2)
  require(candidate_uops.length == numIssueSlots+2)
  for (i <- 0 until numIssueSlots+2) {
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
}
