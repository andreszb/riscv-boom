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


class ShadowBufferIo(implicit p: Parameters) extends BoomBundle()(p) {
  // From ROB
  val rob_enq = Input(Bool())
  val rob_commit_uop = Input(UInt(SB_ADDR_SZ.W))
  val rob_commit_valid = Input(Bool())

  //
  val sb_tail = Output(UInt(SB_ADDR_SZ.W))
  val sb_head = Output(UInt(SB_ADDR_SZ.W))

  val sb_full = Output(Bool())
  val sb_empty = Output(Bool())
}


class ShadowBuffer(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new ShadowBufferIo)


  // Tail and head pointers are registers
  val sb_tail     = RegInit(0.U(SB_ADDR_SZ.W))
  val sb_head     = RegInit(0.U(SB_ADDR_SZ.W))

  // Actual buffer. Only need 1 bit per entry T/F
  val sb_data      = Mem(NUM_SB_ENTRIES, Bool())


  when (io.rob_enq)
  {
    sb_data(sb_tail) := true.B
    sb_tail := sb_tail + 1.U

    // Wrap around (its a circular buffer
    when (sb_tail === NUM_SB_ENTRIES.U)
    {
      sb_tail := 0.U(SB_ADDR_SZ.W)
    }

    when (sb_tail === sb_head) {
      io.sb_full := true.B
    }

    if (DEBUG_PRINTF) {
      printf("SB received rob_enq signal. tail=%d\n", sb_tail)
    }
  }
  when (io.rob_commit_valid)
  {
    sb_data(io.rob_commit_uop) := false.B
    if (DEBUG_PRINTF) {
      printf("SB received commit signal for idx=%d\n", io.rob_commit_uop)
    }

  }

  io.sb_tail := sb_tail
  io.sb_head := sb_head



}


