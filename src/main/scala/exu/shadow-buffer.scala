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

import boom.common._
import boom.util._

import freechips.rocketchip.config.Parameters


class ShadowBufferIo(
  val machine_width:Int,
  val num_wakeup_ports:Int
)(implicit p: Parameters) extends BoomBundle()(p) {
  // From ROB
  val rob_enq = Input(Vec(machine_width, Bool()))
  val rob_commit_uop = Input(Vec(machine_width, Vec(num_wakeup_ports, UInt(SB_ADDR_SZ.W))))
  val rob_commit_valid = Input(Vec(machine_width,Vec(num_wakeup_ports, Bool())))

  // To Rob
  val rob_q_idx = Output(Vec(machine_width, UInt(SB_ADDR_SZ.W)))

  val sb_tail = Output(UInt(SB_ADDR_SZ.W))
  val sb_head = Output(UInt(SB_ADDR_SZ.W))

  val sb_full = Output(Bool())
}


class ShadowBuffer(
  width: Int,
  num_wakeup_ports: Int
)(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new ShadowBufferIo(width, num_wakeup_ports))


  // Tail and head pointers are registers
  val sb_tail     = RegInit(0.U(SB_ADDR_SZ.W))
  val sb_head     = RegInit(0.U(SB_ADDR_SZ.W))
  val sb_full     = RegInit(false.B)

  // Actual buffer. Only need 1 bit per entry T/F
  val sb_data      = Mem(NUM_SB_ENTRIES, Bool())

  // We need 1 wire per port to do the calculation of index to send back
  val rob_q_idx = Wire(Vec(width, UInt(SB_ADDR_SZ.W)))

  // Handle dispatch
  for (w <- 0 until width)
  {
    if (w == 0) {
      rob_q_idx(w) := sb_tail
    } else {
      // Here we calculate the q idx to pass back to the ROB
      rob_q_idx(w) := Mux(io.rob_enq(w-1), rob_q_idx(w-1)+1.U,rob_q_idx(w-1))
    }
    // Write to the SB buffer modulus ensures wrap-around
    when(io.rob_enq(w)) {
      sb_data.write((rob_q_idx(w) % SB_ADDR_SZ.U) , true.B)
    }
    // Expose the shadow buffer index to the ROB
    io.rob_q_idx(w) := rob_q_idx(w)

    // Handle commits
    for(i <- 0 until num_wakeup_ports)
    {
      when(io.rob_commit_valid(w)(i))
      {
        sb_data.write(io.rob_commit_uop(w)(i), false.B)
      }
    }
  }

  
  // Calculate next SB_TAIL
  val sb_tail_next =(rob_q_idx(width-1) + 1.U) % SB_ADDR_SZ.U
  val sb_head_next = sb_head
  // Calculate next SB_HEAD
  when(sb_data.read(sb_head) === false.B) 
  {
    val sb_head_next = (sb_head - 1.U) % SB_ADDR_SZ.U
  }

  // Check if we are "full". This is sub-optimal but to reduce complexity
  // Also this is bugprone and needs to be safeguarded wrt SB_ADDR_SZ
  when((sb_tail_next + width.U) % SB_ADDR_SZ.U >= sb_head_next)
  {
    sb_full := true.B
  }.otherwise
  {
    sb_full := false.B
  }

  sb_tail := sb_tail_next
  sb_head := sb_head_next

  io.sb_full := sb_full
  io.sb_tail := sb_tail
  io.sb_head := sb_head


}


