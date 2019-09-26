//******************************************************************************
// Copyright (c) 2019
//------------------------------------------------------------------------------
// Author: erlingrj@stud.ntnu.no
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Shadow Buffer
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util.Valid

import boom.common._
import boom.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.Parameters


/**
 * The Release Queue is closely connectd to the Shadow Buffer and the LSU. It tracks speculative loads in the Load Queue
 * and the instruction which makes it speculative. It updates the Load Queue by 2 Valid interfaces to set/unset a bit in
 * the load queue.
 * @param machine_width
 * @param p
 *
 */

class RQEnqSignals(implicit p: Parameters) extends BoomBundle()(p) {
  val valid = Bool()
  val ldq_idx = UInt(ldqAddrSz.W)
}


class ReleaseQueueIo(
                    val machine_width:Int
                    )(implicit p: Parameters) extends BoomBundle()(p)
{
  // From ROB
  val enq = Input(Vec(machine_width, new RQEnqSignals()))


  // From ShadowBuffer
  val commit = Input(new SBCommitSignals())
  val sb_tail = Input(UInt(sbAddrSz.W))

  // To LSU
  // TODO combine this to 1 single interface
  val set_shadow_bit = Output(Vec(coreWidth, Valid(UInt(ldqAddrSz.W))))
  val unset_shadow_bit = Output(Vec(rqCommitWidth, Valid(UInt(ldqAddrSz.W))))

}

class ReleaseQueue(
                  width:Int
                  )(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new ReleaseQueueIo(width))

  //Head and Tail to the buffer
  val tail  = RegInit(0.U(rqAddrSz.W))
  val head  = RegInit(0.U(rqAddrSz.W))
  val full  = RegInit(false.B)

  // Actual buffer LQ_idx, SB_idx and valid
  val ldq_idx = Reg(Vec(numRqEntries, UInt(ldqAddrSz.W))) //LDQ Index for the speculative load
  val sb_idx = Reg(Vec(numRqEntries, UInt(sbAddrSz.W))) //ShadowBuffer index for the instruction shadowing the load
  val is_speculative = RegInit(VecInit(Seq.fill(numRqEntries) {false.B})) //Is the ShadowBuffer instruction still speculative
  val was_killed = RegInit(VecInit(Seq.fill(numRqEntries) {false.B}))
  val valid = RegInit(VecInit(Seq.fill(numRqEntries){false.B})) //Is the entry still valid (set to false when the LSU is informed)


  // Intermediate calculation of the idx into the ReleaseQueue where we will actually store the data
  val q_idx = Wire(Vec(width, UInt(rqAddrSz.W)))

  // Handle enques from ROB
  for (w <- 0 until width)
    {
      if(w == 0) {
        q_idx(w) := tail
      } else {
        q_idx(w) := Mux(io.enq(w-1).valid, WrapInc(q_idx(w-1), numRqEntries), q_idx(w - 1))
      }
      when(io.enq(w).valid) {
        valid(q_idx(w)) := true.B
        is_speculative(q_idx(w)) := true.B
        ldq_idx(q_idx(w)) := io.enq(w).ldq_idx
        sb_idx(q_idx(w)) := WrapDec(io.sb_tail, numSbEntries)

        // Send info to Load Queue
        io.set_shadow_bit(w).valid := true.B
        io.set_shadow_bit(w).bits := io.enq(w).ldq_idx
      }.otherwise
      {
        io.set_shadow_bit(w).valid := false.B
      }
    }

  // Handle commits from the ShadowBuffer. Flip the is_speculative bits
  when(io.commit.valid) {
    for (i <- 0 until numRqEntries)
      {
        when(sb_idx(i) === io.commit.sb_idx)
         {
           is_speculative(i) := false.B
           was_killed(i) := io.commit.killed
         }
      }
  }

  // Handle updates to LSU and update head
  // TODO code review. This could be done more elgant with scala var
  val head_next = WireInit(head)
  for(i <- 0 until rqCommitWidth)
    {
      val idx = WrapAdd(head, i.U, numRqEntries) //TODO solution using WrapInc and not Add
      when(valid(idx) && !is_speculative(idx) && !was_killed(idx)){
        head_next := WrapInc(idx, numRqEntries)
        io.unset_shadow_bit(i).valid := true.B
        io.unset_shadow_bit(i).bits := ldq_idx(idx)
      }.elsewhen(valid(idx) && was_killed(idx)) { //We are committing a killed shadow caster
                                                  // LSU already has killed this load
        head_next := WrapInc(idx, numRqEntries)
        io.unset_shadow_bit(i).valid := false.B // TODO do we wanna keep the write port idle?
        }.otherwise {
        io.unset_shadow_bit(i).valid := false.B
      }
    }


  // Update tail
  val tail_next = WireInit(tail)
  when(valid(q_idx(width-1))) {
    tail_next := WrapInc(q_idx(width-1), numRqEntries)
  }.otherwise {
    tail_next := q_idx(width-1)
  }

  // Check if we are pseudo-full
  when((WrapSub2HW(head_next, tail_next, numRqEntries) <= width.U) && valid(head_next))
  {
    full := true.B
  }.otherwise
  {
    full := false.B
  }

  // Route signals out
  tail := tail_next
  head := head_next


  // DontTouch for debugs
  dontTouch(head)
  dontTouch(tail)
  dontTouch(full)
  dontTouch(io.enq)
  dontTouch(io.commit)
  dontTouch(io.set_shadow_bit)
  dontTouch(io.unset_shadow_bit)
  dontTouch(q_idx)
  dontTouch(tail_next)
  dontTouch(head_next)

}