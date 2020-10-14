package boom.exu

import boom.common.BoomModule
import chipsalliance.rocketchip.config.Parameters
import chisel3._

//This one is done
class ShadowBuffer(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val new_branch_op = Input(Vec(coreWidth, Bool()))

    val no_longer_speculative_index_in = Input(Vec(coreWidth, UInt(8.W)))
    val no_longer_speculative_committed = Input(Vec(coreWidth, Bool()))

    val shadow_buffer_head_out = Output(UInt(8.W))
    val shadow_buffer_tail_out = Output(UInt(8.W))
  }

  //Remember: Head is oldest speculative op, Tail is newest speculative op
  val ShadowBufferHead = RegInit(UInt(8.W), 0.U)
  val ShadowBufferTail = RegInit(UInt(8.W), 0.U)

  val ShadowCaster = Reg(Vec(64, Bool()))

  io.shadow_buffer_head_out := ShadowBufferHead
  io.shadow_buffer_tail_out := ShadowBufferTail

  for (w <- 0 until coreWidth) {
    when(io.new_branch_op(w)) {
      ShadowBufferTail := (ShadowBufferTail + 1.U) % 64.U
      ShadowCaster(ShadowBufferTail) := true.B
    }

    when(io.no_longer_speculative_committed(w)) {
      ShadowCaster(io.no_longer_speculative_index_in(w)) := false.B
    }

    when(ShadowCaster(ShadowBufferHead) === false.B) {
      ShadowBufferHead := (ShadowBufferHead + 1.U) % 64.U
    }
  }

}
