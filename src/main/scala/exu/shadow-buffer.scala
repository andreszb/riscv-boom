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


class SBCommitSignals(implicit p: Parameters) extends BoomBundle()(p)
{
  val sb_idx = UInt(sbAddrSz.W)
  val killed = Bool()
  val valid = Bool()
}

class ShadowBufferIo(
  val machine_width:Int,
  val num_wakeup_ports:Int
)(implicit p: Parameters) extends BoomBundle()(p) {
  
  // From ROB
  val enq_uop = Input(Vec(machine_width, Flipped(Valid(new MicroOp()))))
  val wb_uop = Input(Vec(num_wakeup_ports, Flipped(Valid(UInt(sbAddrSz.W)))))
  val kill = Flipped(Valid(UInt(sbAddrSz.W)))


  // To Rob
  val q_idx = Output(Vec(machine_width, UInt(sbAddrSz.W)))
  
  val tail = Output(UInt(sbAddrSz.W)) // The latest instruction added
  val tail_spec = Output(UInt(sbAddrSz.W)) //The latest still speculative instruction. RQ needs this to store
  val head = Output(UInt(sbAddrSz.W))
  val empty = Output(Bool())
  val full = Output(Bool())

  // From Branch Unit
  val brinfo = Input(new BrResolutionInfo())

  // To Release Queue
  val release = Output(new SBCommitSignals())
}


class ShadowBuffer(
  width: Int,
  num_wakeup_ports: Int
)(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new ShadowBufferIo(width, num_wakeup_ports))


  // Tail and head pointers are registers
  val tail     = RegInit(0.U(sbAddrSz.W))
  val head     = RegInit(0.U(sbAddrSz.W))
  val full     = RegInit(false.B)
  val empty    = WireInit(true.B)

  // Actual buffer. Only need 2 bit per entry T/F and valid/not-valid
  // True/False is wether the instruction is still speculative
  // valid/not-valid is wether it is dispatched and not committed yet
  // The first implementation only supports one committ port from the Shadow Buffer
  val sb_data      = RegInit(VecInit(Seq.fill(numSbEntries){false.B}))
  val sb_valid     = RegInit(VecInit(Seq.fill(numSbEntries){false.B}))
  val sb_uop       = Reg(Vec(numSbEntries, new MicroOp()))
  val sb_killed    = RegInit(VecInit(Seq.fill(numSbEntries){false.B}))

  // We need 1 wire per port to do the calculation of index to send back
  val q_idx = Wire(Vec(width, UInt(sbAddrSz.W)))

  // Handle dispatch
  for (w <- 0 until width) {
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

  // Kil entries on ROB branch except
  when (io.kill.valid) {
    val idx = io.kill.bits
    sb_data(idx) := false.B
    sb_uop(idx).inst := BUBBLE
    sb_killed(idx)    := true.B
  }

  // Update buffer pointers and full/empty
  // Calculate next tail. It only depends on if we have enqueued new instructions this CC
  val tail_next = Mux(io.enq_uop(width-1).valid, WrapInc(q_idx(width-1), numSbEntries), q_idx(width-1))
  val head_next = Mux(!sb_data(head) && sb_valid(head), WrapInc(head, numSbEntries), head)

  // The Head depends on multiple factors:
  // 1. Have we committed some shadow-casters?
  // 2. Have we killed som shadow-casters that were speculated under other instructions?
  // When incrementing the head we also
  // Calculate next head and also dealing with committing to Release Queue.


  when(!sb_data(head) && sb_valid(head))
  {
    // Commit the current head
    sb_valid(head) := false.B
    head := WrapInc(head, numSbEntries)
    io.release.valid := true.B
    io.release.killed := sb_killed(head)
    io.release.sb_idx := head
  }.otherwise {
    io.release.valid := false.B
  }


  // Calculate the speculative tail. That is, the latest speculative instruction in the buffer.
  // Any newly arriving load will be shadowed by that instruction.
  // TODO: Combine this with checking for empty?
  val idx = Wire(Vec(numSbEntries, UInt(sbAddrSz.W)))
  idx(0) := head
  for(i <- 0 until numSbEntries)
    {
      if(i == 0)
        {
          when(sb_data(idx(i)) && sb_valid(idx(i))) {
            io.tail_spec := idx(i)
          }
        } else
        {
          idx(i) := WrapInc(idx(i-1), numSbEntries)
          when(sb_data(idx(i)) && sb_valid(idx(i))) {
            io.tail_spec := idx(i)
          }
        }
    }

  // Check if we are "full". This is sub-optimal but to reduce complexity
  // Also this is bugprone and needs to be safeguarded wrt sbAddrSz

  when((WrapSub2HW(head_next, tail_next, numSbEntries) <= width.U) && sb_valid(head_next))
  {
    full := true.B
  }.otherwise
  {
    full := false.B
  }

  // Check if we are "empty" of shadowing instructions
  empty := true.B
  for (w <- 0 until numSbEntries)
    {
      when(sb_data(w) && sb_valid(w))
      {
        empty := false.B
      }
    }

  tail := tail_next


  io.full := full
  io.tail := tail
  io.head := head
  io.empty := empty


  // DONTTOUCH FOR DEBUGS
  dontTouch(head)
  dontTouch(tail)
  dontTouch(tail_next)
  dontTouch(head_next)
  dontTouch(full)
  dontTouch(io.enq_uop)
  dontTouch(io.wb_uop)
  dontTouch(io.q_idx)
  dontTouch(io.head)
  dontTouch(io.tail)
  dontTouch(io.full)
  dontTouch(empty)
  dontTouch(io.release)
  dontTouch(io.kill)
}



