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
import chisel3.util.PopCount
import freechips.rocketchip.config.Parameters

/**
 * Specific type of issue unit
 *
 * @param params issue queue params
 * @param numWakeupPorts number of wakeup ports for the issue queue
 */
class IssueUnitIno(
  params: IssueParams,
  numWakeupPorts: Int)
                  (implicit p: Parameters)
  extends IssueUnit(params.numEntries, params.issueWidth, numWakeupPorts, params.iqType, params.dispatchWidth)
{
  //-------------------------------------------------------------
  // Figure out how much to shift entries by

  require(params.numEntries == params.dispatchWidth)

  val maxShift = dispatchWidth
  // a bit hacky to use will_be_valid - could affect critical path
  val vacants = issue_slots.map(s => !(s.will_be_valid)) ++ io.dis_uops.map(_.valid).map(!_.asBool)
  val shamts_oh = Array.fill(numIssueSlots+dispatchWidth) {Wire(UInt(width=maxShift.W))}
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
  for (i <- 1 until numIssueSlots + dispatchWidth) {
    shamts_oh(i) := SaturatingCounterOH(shamts_oh(i-1), vacants(i-1), maxShift)
  }

  //-------------------------------------------------------------

  // which entries' uops will still be next cycle? (not being issued and vacated)
  val will_be_valid = (0 until numIssueSlots).map(i => issue_slots(i).will_be_valid) ++
                      (0 until dispatchWidth).map(i => io.dis_uops(i).valid &&
                                                        !dis_uops(i).exception &&
                                                        !dis_uops(i).is_fence &&
                                                        !dis_uops(i).is_fencei)

  val dispatch_used = WireInit(VecInit(Seq.fill(dispatchWidth)(false.B)))
  dontTouch(dispatch_used)
  val uops = issue_slots.map(s=>s.out_uop) ++ dis_uops.map(s=>s)
  for (i <- 0 until numIssueSlots) {
    issue_slots(i).in_uop.valid := false.B
    issue_slots(i).in_uop.bits  := uops(i+1)
    for (j <- 1 to maxShift by 1) {
      when (shamts_oh(i+j) === (1 << (j-1)).U) {
        issue_slots(i).in_uop.valid := will_be_valid(i+j)
        issue_slots(i).in_uop.bits  := uops(i+j)
        if(i+j >= numIssueSlots){
          dispatch_used(i+j-numIssueSlots) := true.B
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

  val will_be_available = (0 until numIssueSlots).map(i =>
                            (!issue_slots(i).will_be_valid || issue_slots(i).clear) && !(issue_slots(i).in_uop.valid))
  val num_available = PopCount(will_be_available)
  for (w <- 0 until dispatchWidth) {
    io.dis_uops(w).ready := dispatch_used(w)//RegNext(num_available > w.U)
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

  val requests = issue_slots.map(s => s.request)
  val port_issued = Array.fill(issueWidth){Bool()}
  for (w <- 0 until issueWidth) {
    port_issued(w) = false.B
  }

  val grants = issue_slots.map(_.grant)
  val valids = issue_slots.map(_.valid)
  val candidate_uops = issue_slots.map(_.uop)
  var previous_stalled = false.B

  for (i <- 0 until numIssueSlots) {
    grants(i) := false.B
    val port_reserved = Array.fill(issueWidth) {false.B}
    val uop_iq_ports_satisfied = Wire(Vec(IQT_SZ, Bool()))
    for (n <- 0 until IQT_SZ) {
      val iq_type = 1.U(1.W) << n
      when((iq_type & candidate_uops(i).iq_type) =/= 0.U) {
        var uop_issued = false.B

        for (w <- 0 until issueWidth) {
          val iq_type_match = (iq_type === io.iq_types.get(w))
          val fu_type_match = ((candidate_uops(i).fu_code & io.fu_types(w)) =/= 0.U)
          iq_type_match.suggestName(s"iq_type_match_slot_${i}_exu_$w")
          fu_type_match.suggestName(s"fu_type_match_slot_${i}_exu_$w")
          val can_allocate = fu_type_match && iq_type_match
          val was_port_issued_yet = port_issued(w)
          port_reserved(w) = (requests(i) && !uop_issued && can_allocate && !was_port_issued_yet) | port_reserved(w)
          uop_issued = (requests(i) && can_allocate && !was_port_issued_yet) | uop_issued
        }
        uop_iq_ports_satisfied(n) := uop_issued
      }.otherwise {
        uop_iq_ports_satisfied(n) := true.B
      }
    }
    // Now check if this uop got all the iq ports it needed
    val satisfied = uop_iq_ports_satisfied.reduce(_ && _)
    val can_issue = satisfied && !previous_stalled
    if(boomParams.inoParams.exists(_.stallOnUse)){
      previous_stalled = (!satisfied && valids(i)) || previous_stalled
    }
    when (can_issue) {
      grants(i) := true.B
      for (w <- 0 until issueWidth) {
        port_issued(w) = port_issued(w) | port_reserved(w)
        when(port_reserved(w)) {
          io.iss_valids(w) := true.B
          io.iss_uops(w) := candidate_uops(i)
        }
      }
      assert(PopCount(port_reserved) === PopCount(candidate_uops(i).iq_type), "[cas-ino] issues at more ports than it should")
    }
    when(candidate_uops(i).iq_type === IQT_MFP) {
      assert(! (grants(i) && !(PopCount(port_reserved) === 2.U) ), "[cas-ino] fsd didnt reserve 2 ports")
      assert(!( grants(i) && !(PopCount(io.iss_uops.map(_.inst === candidate_uops(i).inst)) === 2.U )), "[cas-ino] fsd went wrong")
    }
  }

  assert(!(PopCount(grants) > issueWidth.U), "[iss-ino] More grants than issue ports")
}
