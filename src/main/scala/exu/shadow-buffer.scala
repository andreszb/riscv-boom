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


class ShadowBufferIo(
  val machine_width:Int,
  val num_wakeup_ports:Int
)(implicit p: Parameters) extends BoomBundle()(p) {
  
  // From ROB
  val enq_uop = Input(Vec(machine_width, Bool()))
  val commit_uop = Input(Vec(machine_width, Vec(num_wakeup_ports, Flipped(Valid(UInt(SB_ADDR_SZ.W))))))

  // To Rob
  val q_idx = Output(Vec(machine_width, UInt(SB_ADDR_SZ.W)))
  
  val tail = Output(UInt(SB_ADDR_SZ.W))
  val head = Output(UInt(SB_ADDR_SZ.W))
  
 // val empty = Output(Bool())
  val full = Output(Bool())
}


class ShadowBuffer(
  width: Int,
  num_wakeup_ports: Int
)(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new ShadowBufferIo(width, num_wakeup_ports))


  // Tail and head pointers are registers
  val tail     = RegInit(0.U(SB_ADDR_SZ.W))
  val head     = RegInit(0.U(SB_ADDR_SZ.W))
  val full     = RegInit(false.B)

  // Actual buffer. Only need 1 bit per entry T/F
  val sb_data      = Mem(NUM_SB_ENTRIES, Bool())

  // We need 1 wire per port to do the calculation of index to send back
  val q_idx = Wire(Vec(width, UInt(SB_ADDR_SZ.W)))

  // Handle dispatch
  for (w <- 0 until width)
  {
    if (w == 0) {
      q_idx(w) := tail
    } else {
      // Here we calculate the q idx to pass back to the ROB
      q_idx(w) := Mux(io.enq_uop(w-1), WrapInc(q_idx(w-1), NUM_SB_ENTRIES), q_idx(w-1))
    }
    // Write to the SB buffer modulus ensures wrap-around
    when(io.enq_uop(w)) {
      sb_data.write((q_idx(w)) , true.B)
    }
    // Expose the shadow buffer index to the ROB
    io.q_idx(w) := q_idx(w)

    // Handle commits
    for(i <- 0 until num_wakeup_ports)
    {
      when(io.commit_uop(w)(i).valid)
      {
        sb_data.write(io.commit_uop(w)(i).bits, false.B)
      }
    }
  }

  
  // Calculate next tail
  val tail_next = Mux(io.enq_uop(width-1), WrapInc(q_idx(width-1), NUM_SB_ENTRIES), q_idx(width-1))
  val head_next = head
  // Calculate next head
  when(sb_data(head) === false.B)
  {
    val head_next = WrapDec(head, NUM_SB_ENTRIES)
  }

  // Check if we are "full". This is sub-optimal but to reduce complexity
  // Also this is bugprone and needs to be safeguarded wrt SB_ADDR_SZ
  when(WrapAdd(tail_next, width.U, NUM_SB_ENTRIES) >= head_next)
  {
    full := true.B
  }.otherwise
  {
    full := false.B
  }

  tail := tail_next
  head := head_next

  io.full := full
  io.tail := tail
  io.head := head


  // DONTTOUCH FOR DEBUGS
  dontTouch(head)
  dontTouch(tail)
  dontTouch(tail_next)
  dontTouch(head_next)
  dontTouch(full)
  dontTouch(io.enq_uop)
  dontTouch(io.commit_uop)
  dontTouch(io.q_idx)
}



