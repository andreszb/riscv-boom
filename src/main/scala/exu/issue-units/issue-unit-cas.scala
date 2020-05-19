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
import chisel3.util
import freechips.rocketchip.config.Parameters

/**
  * Specific type of issue unit
  *
  * @param params issue queue params
  * @param numWakeupPorts number of wakeup ports for the issue queue
  */
@chiselName
class IssueUnitQueuesUnified(
                           params: IssueParams,
                           numWakeupPorts: Int)
                            (implicit p: Parameters)
  extends IssueUnit(params.numEntries, params.issueWidth, numWakeupPorts, params.iqType, params.dispatchWidth)
{
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
  val requests = (io.q1_heads.get ++ io.q2_heads.get).map(h => (h.valid &&
    !h.bits.exception &&
    !h.bits.is_fence &&
    !h.bits.is_fencei
    ) && uop_ready(h.bits))

  // Issue Unit Grants are connected to the ready signals => signalling dequeue
  val grants =  io.q1_heads.get.map(h => h.ready) ++
                io.q2_heads.get.map(h => h.ready)

  val candidate_uops = io.q1_heads.get.map(h => h.bits) ++ io.q2_heads.get.map(h => h.bits)
  val port_issued = Array.fill(issueWidth){false.B}


  val len = candidate_uops.length

  dontTouch(io.iq_types.get)
  dontTouch(io.fu_types)
  for (i <- 0 until len) {
    grants(i) := false.B
    val port_reserved = Array.fill(issueWidth) {false.B}
    val uop_iq_ports_satisfied = Wire(Vec(IQT_SZ, Bool()))
    for (n <- 0 until IQT_SZ) {
      val iq_type = 1.U(1.W) << n
      val iq_present = ((iq_type & candidate_uops(i).iq_type) =/= 0.U)
      var uop_issued = false.B

      for (w <- 0 until issueWidth) {
        val iq_type_match = (iq_type === io.iq_types.get(w)) && iq_present
        val fu_type_match = ((candidate_uops(i).fu_code & io.fu_types(w)) =/= 0.U)
        iq_type_match.suggestName(s"iq_type_match_slot_${i}_exu_$w")
        fu_type_match.suggestName(s"fu_type_match_slot_${i}_exu_$w")
        val can_allocate = fu_type_match && iq_type_match
        val was_port_issued_yet = port_issued(w)
        port_reserved(w) = (requests(i) && !uop_issued && can_allocate && !was_port_issued_yet) | port_reserved(w)
        uop_issued = (requests(i) && can_allocate && !was_port_issued_yet) | uop_issued
      }
      when(iq_present){
        uop_iq_ports_satisfied(n) := uop_issued
      }.otherwise {
        uop_iq_ports_satisfied(n) := true.B
      }
    }
    // Now check if this uop got all thw iq ports it needed
    val can_issue = uop_iq_ports_satisfied.reduce(_ && _)
    when (can_issue) {
      grants(i) := true.B
      for (w <- 0 until issueWidth) {
        port_issued(w) = port_issued(w) | port_reserved(w)
        when(port_reserved(w)) {
          io.iss_valids(w) := true.B
          io.iss_uops(w) := candidate_uops(i)
        }
        port_reserved(w).suggestName(s"port_reserved_${i}_${w}")
      }
      assert(PopCount(port_reserved) === PopCount(candidate_uops(i).iq_type), "[cas-iss] issues at more ports than it should")
    }
    when(candidate_uops(i).iq_type === IQT_MFP) {
      assert(! (grants(i) && !(PopCount(port_reserved) === 2.U) ), "[cas-iss] fsd didnt reserve 2 ports")
//      assert(!( grants(i) && !(PopCount(io.iss_uops.map(_.debug_events.fetch_seq === candidate_uops(i).debug_events.fetch_seq)) === 2.U )), "[cas-iss] fsd went wrong")
    }
  }

  assert(!(PopCount(grants) > issueWidth.U), "[iss-cas] More grants than issue ports")
}
