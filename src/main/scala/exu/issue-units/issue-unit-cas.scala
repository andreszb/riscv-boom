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
import freechips.rocketchip.config.Parameters

/**
  * Specific type of issue unit
  *
  * @param params issue queue params
  * @param numWakeupPorts number of wakeup ports for the issue queue
  */
@chiselName
class IssueUnitCasUnified(
                           params: IssueParams,
                           numWakeupPorts: Int)
                         (implicit p: Parameters)
  extends IssueUnit(params.numEntries, params.issueWidth, numWakeupPorts, params.iqType, params.dispatchWidth)
{

}
