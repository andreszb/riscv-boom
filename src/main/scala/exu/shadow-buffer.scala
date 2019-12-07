//******************************************************************************
// Copyright (c) 2019
//------------------------------------------------------------------------------
// Author: erlingrj@stud.ntnu.no
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Shadow Buffer
//  The Shadow Buffer (SB) is part of a technique closing one of the covert
//  channels for leaking secrets that are used in Spectre-like attacks
//  SB is a ring buffer that is used to track instructions that cast a
//  speculative shadow (e.g. branches, jumps). New entries are enqueued by the ROB
//  and they are resolved from both the ROB and the Branch Unit.
//  The Release Queue tags loads with the youngest shadowcaster. (I.e. the speculative
//  tail of the SB. The SB commits in-order to the Release Queue that releases
//  any loads speculated under that shadow-casting instruction.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util.Valid

import boom.common._
import boom.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.Parameters


/* SBCommitSignals are the interface from ShadowBuffer to Release Queue.
*/
class SBCommitSignals(implicit p: Parameters) extends BoomBundle()(p)
{
  val sb_idx = UInt(sbAddrSz.W) //Index of the committed instruction
  val killed = Bool() //Wether it was killed
  val valid = Bool()
}

class ShadowBufferIo(
  val num_wakeup_ports:Int
)(implicit p: Parameters) extends BoomBundle()(p) {
  
  // From ROB
  val enq_uop = Input(Vec(coreWidth, Flipped(Valid(new MicroOp())))) // ROB enqueues new Shadow Casting instrucntio
  val wb_uop = Input(Vec(num_wakeup_ports, Flipped(Valid(UInt(sbAddrSz.W))))) // WB from ROB
  val rollback = Input(Vec(coreWidth, Flipped(Valid(UInt(sbAddrSz.W)))))    // ROB kills Shadow Caster during Rollback


  // To Rob
  val q_idx = Output(Vec(coreWidth, UInt(sbAddrSz.W)))                  // SB queue idx of the most recent enque
  val tail = Output(UInt(sbAddrSz.W)) // Tail of the ring buffer
  val tail_spec = Output(UInt(sbAddrSz.W)) //The latest still speculative instruction. Any new loads will be tagged with this index
  val head = Output(UInt(sbAddrSz.W)) // Head of the ring buffer
  val empty = Output(Bool())
  val full = Output(Bool())

  // From Branch Unit
  val brinfo = Input(new BrResolutionInfo()) // The actual writebacks for branches come through here.

  // To Release Queue
  val release = Output(Vec(sbRqCommitWidth, new SBCommitSignals())) //Update Release Queue
}


class ShadowBuffer(
  num_wakeup_ports: Int
)(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new ShadowBufferIo(num_wakeup_ports))

  // wrapIndex maps an integer i to the index in the ring buffer
  def wrapIndex(i: Int): UInt = {
    val out = Wire(UInt(sbAddrSz.W))
    when((i.U + head) <= numSbEntries.U) {
      out := i.U + head
    }.otherwise {
      out := (i.U + head) - numSbEntries.U
    }
    out
  }


  // Tail and head pointers are registers
  val tail     = RegInit(0.U(sbAddrSz.W))
  val head     = RegInit(0.U(sbAddrSz.W))
  val full     = RegInit(false.B)
  val empty    = WireInit(true.B)

  // Speculative tail pointer is a wire
  val tail_spec= WireInit(0.U(sbAddrSz.W))

  // Each entry in the SB consists of:
  //  sb_val: Is the entry valid
  //  sb_spec: Is the entry currently speculative
  //  sb_uop: The instruction in question (used to get the branch mask)
  //  sb_killed: Was the instruction killed/did it kill everyone under its shadow
  // TODO: Construct a single object from this?
  val sb_val     = RegInit(VecInit(Seq.fill(numSbEntries){false.B}))
  val sb_spec      = RegInit(VecInit(Seq.fill(numSbEntries){false.B}))
  val sb_uop       = Reg(Vec(numSbEntries, new MicroOp()))
  val sb_killed    = RegInit(VecInit(Seq.fill(numSbEntries){false.B}))


  //--------------------------------------------------------------------
  // Dispatch: Add Entry to SB
  //  ROB will enqueue up to n instructions per CC. We add them to the SB, in order, and return
  //  the index to where they where stored on the corresponding output port (io.q_idx)

  // q_idx is a wire that holds the sb_idx for the enqueued instruction at the corresponing enq port.
  val q_idx = Wire(Vec(coreWidth, UInt(sbAddrSz.W)))
  for (w <- 0 until coreWidth) {
    if (w == 0) {
      q_idx(w) := tail
    } else {
      // Here we calculate the q idx to pass back to the ROB
      q_idx(w) := Mux(io.enq_uop(w - 1).valid, WrapInc(q_idx(w - 1), numSbEntries), q_idx(w - 1))
    }
    // Write to the Shadow Buffer
    when(io.enq_uop(w).valid) {
      sb_killed(q_idx(w)) := false.B
      sb_spec(q_idx(w)) := true.B
      sb_val(q_idx(w)) := true.B
      sb_uop(q_idx(w))   := io.enq_uop(w).bits
    }
    // Expose the shadow buffer index to the ROB
    io.q_idx(w) := q_idx(w)
  }

  //--------------------------------------------------------------------
  // Writeback: ROB writes back the finished instructions

  // TODO: Realize that the "branch wb" really is happening through the BRU. Should maybe not writeback here
  for(i <- 0 until num_wakeup_ports)
  {
    when(io.wb_uop(i).valid)
    {
      sb_spec(io.wb_uop(i).bits.asUInt()) := false.B
    }
  }
  //--------------------------------------------------------------------
  // Branch Resolution: Branch Unit deals with branch resolution and signals
  //  all affected modules when a branch is resolved. SB reads the

  // TODO: Work around for avoiding referencing rob_idx here as well? Could moving to  doing everything in ROB and
  //  writing a fixed number of kills per CC by a solution?

  for (i <- 0 until numSbEntries)
    {
      val br_mask = sb_uop(i).br_mask
      val entry_match = io.brinfo.valid && sb_val(i) && maskMatch(io.brinfo.mask, br_mask) // Is this instr shadowed by the branch that has been setteld
      val br_match = io.brinfo.valid && sb_val(i) && (sb_uop(i).rob_idx === io.brinfo.rob_idx) // If it is this very branch that is settled


      when (io.brinfo.mispredict && (entry_match || br_match)) {
        // This entry was speculated under the resolved branch, and it was a mispredict
        //  Or. this entry was the mispredicted branch,
        //  Squash it and record that it was killed
        sb_spec(i) := false.B
        sb_uop(i.U).inst := BUBBLE
        sb_killed(i)    := true.B
      }.elsewhen (!io.brinfo.mispredict && br_match) {
        // The resolved branch is THIS entry and it was predicted correctly
        // => Clear the speculation bit
        sb_spec(i)    := false.B
      }.elsewhen(!io.brinfo.mispredict && entry_match) {
        // This entry was speculated under the resolved branch and it was a correct prediction
        // => Update the branch mask
        sb_uop(i).br_mask := (br_mask & ~io.brinfo.mask)
      }
    }

  //--------------------------------------------------------------------
  // Rollback: Kill entries when ROB performs a rollback


  for (w <- 0 until coreWidth)
    {
      when(io.rollback(w).valid)
      {
        val idx = io.rollback(w).bits
        sb_spec(idx) := false.B
        sb_killed(idx) := true.B
      }
    }

  //--------------------------------------------------------------------
  // Commit and update head/tail: Commit from the head to the Release Queue

  // While committing we also move the head pointer.
  val head_next = WireInit(head)
  val stop = WireInit(VecInit(Seq.fill(sbRqCommitWidth){false.B}))

  for (i <- 0 until sbRqCommitWidth)
    {
      val idx = wrapIndex(i)
      // Since we only commit from the head, we need to cancel if we find a still-speculative
      //  entry.
      val exit = if (i > 0 ) stop(i - 1) else false.B

      when(sb_val(idx) && !sb_spec(idx) && !exit)
      {
        // Commit this instruction to the Release Queue
        io.release(i).valid := true.B
        io.release(i).killed := sb_killed(idx) // If the shadow caster was killed the shadowed loads are also killed
        io.release(i).sb_idx := idx

        sb_val(idx) := false.B // Invalidate this entry
        head_next := WrapInc(idx, numSbEntries) // Increment header
      }.otherwise
      {
        io.release(i).valid := false.B
        stop(i) := true.B
      }
    }

  // Calculate next tail. It only depends on if we have enqueued new instructions this CC
  val tail_next = Mux(io.enq_uop(coreWidth-1).valid, WrapInc(q_idx(coreWidth-1), numSbEntries), q_idx(coreWidth-1))

  //--------------------------------------------------------------------
  // Check full/empty
  //  Also update the speculative tail. I.e. the youngest speculative instr in the SB.
  //  The tail could be a killed branch and then it would be wrong to associate
  //  new loads with that, already killed, instruction.
  for (i <- 0 until numSbEntries) {
    val idx = wrapIndex(i)
    when(sb_spec(idx) && sb_val(idx)) {
      tail_spec := idx
      empty := false.B
    }
  }

  // We are "full" (more like pseudo-full) if we have less available entries than
  //  there are enq ports from the ROB in the next CC.
  when((WrapSub2HW(head_next, tail_next, numSbEntries) <= coreWidth.U) && sb_val(head_next))
  {
    full := true.B
  }.otherwise
  {
    full := false.B
  }

  head := head_next
  tail := tail_next
  io.empty      := empty
  io.full       := full
  io.tail       := tail
  io.tail_spec  := tail_spec
  io.head       := head


  // DONTTOUCH FOR DEBUGS
  dontTouch(head)
  dontTouch(tail)
  dontTouch(head_next)
  dontTouch(full)
  dontTouch(io.enq_uop)
  dontTouch(io.wb_uop)
  dontTouch(io.q_idx)
  dontTouch(io.head)
  dontTouch(io.tail)
  dontTouch(io.tail_spec)
  dontTouch(io.full)
  dontTouch(empty)
  dontTouch(io.release)
  dontTouch(io.rollback)
}



