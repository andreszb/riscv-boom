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

class ShadowBufferIo(implicit p: Parameters) extends BoomBundle()(p) {
  // From ROB
  val rob_enque_uop = Input(Bool())
  val rob_commit_uop = Input(UInt(SB_ADDR_SZ.W))

  //
  val sb_tail = Output(UInt(SB_ADDR_SZ.W))
  val sb_head = Output(UInt(SB_ADDR_SZ.W))

  val sb_full = Output(Bool())
}


class ShadowBuffer(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new ShadowBufferIo)


  // Tail and head pointers are registers
  val sb_tail     = RegInit(0.U(SB_ADDR_SZ.W))
  val sb_head     = RegInit(0.U(SB_ADDR_SZ.W))

  // Actual buffer




}


