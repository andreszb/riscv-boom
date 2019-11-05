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
  val kill = Input(Vec(coreWidth, Flipped(Valid(UInt(sbAddrSz.W)))))    // ROB kills Shadow Caster during Rollback


  // To Rob
  val q_idx = Output(Vec(coreWidth, UInt(sbAddrSz.W)))                  // SB queue idx of the most recent enque
  
  val tail = Output(UInt(sbAddrSz.W)) // The latest instruction added
  val tail_spec = Output(UInt(sbAddrSz.W)) //The latest still speculative instruction. RQ needs this to store
  val head = Output(UInt(sbAddrSz.W))
  val empty = Output(Bool())
  val full = Output(Bool())

  // From Branch Unit
  val brinfo = Input(new BrResolutionInfo()) // The actual writebacks for branches come through here

  // To Release Queue
  val release = Output(Vec(sbRqCommitWidth, new SBCommitSignals())) //Update Release Queue
}


class ShadowBuffer(
  num_wakeup_ports: Int
)(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new ShadowBufferIo(num_wakeup_ports))

  /*
  wrapIndex maps an integer i to the index of the circular buffer.
   */
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

  // Actual buffer
  // TODO: Construct a single object from this?
  val sb_data      = RegInit(VecInit(Seq.fill(numSbEntries){false.B}))
  val sb_valid     = RegInit(VecInit(Seq.fill(numSbEntries){false.B}))
  val sb_uop       = Reg(Vec(numSbEntries, new MicroOp()))
  val sb_killed    = RegInit(VecInit(Seq.fill(numSbEntries){false.B}))

  // We need 1 wire per port to do the calculation of index to send back
  val q_idx = Wire(Vec(coreWidth, UInt(sbAddrSz.W)))

  // Handle dispatch
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
      sb_data(q_idx(w)) := true.B
      sb_valid(q_idx(w)) := true.B
      sb_uop(q_idx(w))   := io.enq_uop(w).bits
    }
    // Expose the shadow buffer index to the ROB
    io.q_idx(w) := q_idx(w)
  }

    // Handle commits
    // TODO: Realize that the "branch wb" really is happening through the BRU. Should maybe not writeback here
    // When we have a branch?
  for(i <- 0 until num_wakeup_ports)
  {
    when(io.wb_uop(i).valid)
    {
      sb_data(io.wb_uop(i).bits.asUInt()) := false.B
    }
  }

  // Kill speculated entries on branch mispredict
  // TODO: Work around for avoiding referencing rob_idx here as well? Could moving to  doing everything in ROB and
  // writing a fixed number of kills per CC by a solution?
  for (i <- 0 until numSbEntries)
    {
      val br_mask = sb_uop(i).br_mask
      val entry_match = sb_valid(i) && maskMatch(io.brinfo.mask, br_mask) // Is this instr shadowed by the branch that has been setteld
      val br_match = sb_valid(i) && (sb_uop(i).rob_idx === io.brinfo.rob_idx) // If it is this very branch that is settled

      //kill instruction if mispredict & br mask match
      when (io.brinfo.valid && io.brinfo.mispredict && (entry_match || br_match))
      {
        sb_data(i) := false.B
        sb_uop(i.U).inst := BUBBLE
        sb_killed(i)    := true.B
      }.elsewhen (io.brinfo.valid && !io.brinfo.mispredict && br_match) {
        // clear speculation bit. It was this very branch that did speculate right
        sb_data(i)    := false.B
        sb_killed(i)  := false.B
      }.elsewhen(io.brinfo.valid && !io.brinfo.mispredict && entry_match) {
        // Clear up the br mask
        sb_uop(i).br_mask := (br_mask & ~io.brinfo.mask)
      }
    }

  // Kill entries on ROB rollback
  for (w <- 0 until coreWidth)
    {
      when(io.kill(w).valid)
      {
        val idx = io.kill(w).bits
        sb_data(idx) := false.B
        sb_killed(idx) := true.B
      }
    }

  // Update buffer pointers and full/empty
  // Calculate next tail. It only depends on if we have enqueued new instructions this CC
  tail := Mux(io.enq_uop(coreWidth-1).valid, WrapInc(q_idx(coreWidth-1), numSbEntries), q_idx(coreWidth-1))


  // Commit from the head to Release Queue and update the head pointer
  val head_next = WireInit(head)
  val stop = WireInit(VecInit(Seq.fill(sbRqCommitWidth){false.B}))

  for (i <- 0 until sbRqCommitWidth)
    {
      val idx = wrapIndex(i)
      val exit = if (i > 0 ) stop(i - 1) else false.B

      when(sb_valid(idx) && !sb_data(idx) && !exit)
      {
        // Commit this instruction to the Release Queue
        io.release(i).valid := true.B
        io.release(i).killed := sb_killed(idx) // If the shadow caster was killed the shadowed loads are also killed
        io.release(i).sb_idx := idx

        sb_valid(idx) := false.B // Invalidate this entry
        head_next := WrapInc(idx, numSbEntries) // Increment header
      }.otherwise
      {
        io.release(i).valid := false.B
        stop(i) := true.B
      }
    }


  // 1: Check if we are empty
  // 2: Update the speculative tail. I.e. the most recent speculative instr.
  // The tail could be a killed branch and then it would be wrong to associate
  // new loads with that, already killed, instruction.
  for (i <- 0 until numSbEntries) {
    val idx = wrapIndex(i)
    when(sb_data(idx) && sb_valid(idx)) {
      tail_spec := idx
      empty := false.B
    }
  }

  // Check if we are "full". This is sub-optimal but to reduce complexity
  // Also this is bugprone and needs to be safeguarded wrt sbAddrSz

  when((WrapSub2HW(head_next, tail, numSbEntries) <= coreWidth.U) && sb_valid(head_next))
  {
    full := true.B
  }.otherwise
  {
    full := false.B
  }

  head := head_next
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
  dontTouch(io.kill)
}



