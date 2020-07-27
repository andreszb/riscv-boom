package boom.exu

import boom.common.BoomModule
import chipsalliance.rocketchip.config.Parameters
import chisel3._

//This one is done
class ReleaseQueue(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val new_entry_in = Input(Bool())

    val shadow_buffer_head_in = Input(UInt())
    val shadow_buffer_tail_in = Input(UInt())
    val load_queue_index_in = Input(UInt())

    val load_queue_index_out = Output(UInt())
    val should_release_load_out = Output(Bool())

  }

  val ShadowStampList = Vec(64, RegInit(UInt(8.W), 0.U))
  val LoadQueueIndexList = Vec(64, RegInit(UInt(8.W), 0.U))

  val ReleaseQueueTail = RegInit(UInt(8.W), 0.U)
  val ReleaseQueueHead = RegInit(UInt(8.W), 0.U)

  io.load_queue_index_out := LoadQueueIndexList(ReleaseQueueTail)
  io.should_release_load_out := false.B

  when(io.shadow_buffer_head_in > ShadowStampList(ReleaseQueueTail)) {
    io.should_release_load_out := true.B
    ReleaseQueueTail := (ReleaseQueueTail + 1.U) % 64.U
  }

  when(io.new_entry_in) {
    ShadowStampList(ReleaseQueueHead) := io.shadow_buffer_tail_in
    LoadQueueIndexList(ReleaseQueueHead) := io.load_queue_index_in
    ReleaseQueueHead := (ReleaseQueueHead + 1.U) % 64.U
  }


}
