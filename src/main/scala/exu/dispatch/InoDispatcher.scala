//******************************************************************************
// Copyright (c) 2012 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// BOOM Instruction Dispatcher
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


package boom.exu

import boom.common.{O3PIPEVIEW_PRINTF, O3_START_CYCLE, uopLD, _}
import chisel3._
import chisel3.internal.naming.chiselName
import freechips.rocketchip.config.Parameters

@chiselName
class InoDispatcher(implicit p: Parameters) extends Dispatcher {
  val dis_stall = WireInit(VecInit(Seq.fill(coreWidth)(false.B)))
  var previous_ready = true.B
  io.dis_uops := DontCare
  io.dis_uops.foreach(_.foreach(_.valid := false.B))
  require(io.dis_uops(LSC_DIS_COMB_PORT_IDX).length == io.ren_uops.length)
  for (i <- 0 until coreWidth) {
    io.dis_uops(LSC_DIS_COMB_PORT_IDX)(i).bits := io.ren_uops(i).bits
    io.dis_uops(LSC_DIS_COMB_PORT_IDX)(i).valid := io.ren_uops(i).valid
    io.ren_uops(i).ready := io.dis_uops(LSC_DIS_COMB_PORT_IDX)(i).ready
  }
}