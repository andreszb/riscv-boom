package lsc
import boom.common._
import chisel3._
import chisel3.core.Bundle
import chisel3.util._
import freechips.rocketchip.config.Parameters

object IbdaParams extends LoadSliceCoreParams {
  // Get tag is a function that calculates the tag from a MicroOp
  def ibda_get_tag(uop: MicroOp): UInt = {
    val tag = Wire(UInt(ibdaTagSz.W))
    if(ibdaTagType == IBDA_TAG_FULL_PC) tag := uop.debug_pc
    else if (ibdaTagType == IBDA_TAG_UOPC_LOB) tag := Cat(uop.uopc, uop.pc_lob)
    else if (ibdaTagType == IBDA_TAG_INST_LOB) tag := Cat(uop.inst, uop.pc_lob)
    tag
  }

}