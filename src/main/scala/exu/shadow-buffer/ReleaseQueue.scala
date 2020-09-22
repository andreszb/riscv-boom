package boom.exu

import boom.common.BoomModule
import chipsalliance.rocketchip.config.Parameters
import chisel3._

//This one is done
class ReleaseQueue(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val new_entry_in = Input(Vec(coreWidth, Bool()))
    val load_queue_index_in = Input(Vec(coreWidth, UInt()))

    val shadow_buffer_head_in = Input(UInt())
    val shadow_buffer_tail_in = Input(UInt())

    val load_queue_index_out = Output(Vec(coreWidth, UInt()))
    val should_release_load_out = Output(Vec(coreWidth, Bool()))

  }

  val ShadowStampList = Reg(Vec(64, RegInit(UInt(8.W), 0.U)))
  val LoadQueueIndexList = Reg(Vec(64, RegInit(UInt(8.W), 0.U)))

  val ReleaseQueueTail = RegInit(UInt(8.W), 0.U)
  val ReleaseQueueHead = RegInit(UInt(8.W), 0.U)

  io.load_queue_index_out := LoadQueueIndexList(ReleaseQueueTail)
  for (i <- 0 until coreWidth) io.should_release_load_out(i) := false.B

  for (w <- 0 until coreWidth) {
    when(io.shadow_buffer_head_in > ShadowStampList(ReleaseQueueTail)) {
      io.should_release_load_out(w) := true.B
      ReleaseQueueTail := (ReleaseQueueTail + 1.U) % 64.U
    }
  }

  for (w <- 0 until coreWidth) {
    when(io.new_entry_in(w)) {
      ShadowStampList(ReleaseQueueHead) := io.shadow_buffer_tail_in
      LoadQueueIndexList(ReleaseQueueHead) := io.load_queue_index_in(w)
      ReleaseQueueHead := (ReleaseQueueHead + 1.U) % 64.U
    }
  }

}
