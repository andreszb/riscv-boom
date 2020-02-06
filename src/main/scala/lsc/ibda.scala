package lsc
import boom.common._
import chisel3._
import chisel3.core.Bundle
import chisel3.util._
import freechips.rocketchip.config.Parameters

object IbdaParams extends LoadSliceCoreParams {
  // Get tag is a function that calculates the tag from a MicroOp
  def ibda_get_tag(uop: MicroOp): UInt = {
    val tag = Wire(UInt(ibda_tag_sz.W))
    if(ibdaTagType == IBDA_TAG_FULL_PC) tag := uop.debug_pc
    else if (ibdaTagType == IBDA_TAG_UOPC_LOB) tag := Cat(uop.uopc, uop.pc_lob)
    else if (ibdaTagType == IBDA_TAG_INST_LOB) tag := Cat(uop.inst, uop.pc_lob)
    else assert(false.B, "ibda_get_tag not implemented for this tag")

    tag
  }

  // Get tag size.
  def ibda_tag_sz() : Int = {
    ibdaTagType match {
      case IBDA_TAG_FULL_PC => 40
      case IBDA_TAG_UOPC_LOB => UOPC_SZ + 6 //uopc + pc_lob
      case IBDA_TAG_INST_LOB => 32 + 6 //inst + pc_lob
      case _ => {
        assert(false.B, "ibda_tag_sz not implemented for this tag")
        0
      }
    }

  }

}