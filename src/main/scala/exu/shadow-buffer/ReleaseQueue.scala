package boom.exu

import Chisel.{Valid, log2Ceil}
import boom.common.BoomModule
import chipsalliance.rocketchip.config.Parameters
import chisel3._

//This one is done
class ReleaseQueue(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val load_queue_index_in = Input(Vec(coreWidth, Valid(UInt(log2Ceil(numLdqEntries).W))))

    val flush_in = Input(Bool())
    val br_mispredict_release_queue_idx = Input(Valid(UInt(log2Ceil(maxBrCount).W)))

    val shadow_buffer_head_in = Input(UInt(log2Ceil(maxBrCount).W))
    val shadow_buffer_tail_in = Input(UInt(log2Ceil(maxBrCount).W))

    val load_queue_index_out = Output(Vec(coreWidth, Valid(UInt())))
    val release_queue_tail_out = Output(UInt(log2Ceil(numLdqEntries).W))
  }

  val ShadowStampList = Reg(Vec(numLdqEntries, Valid(UInt(log2Ceil(maxBrCount).W))))
  val LoadQueueIndexList = Reg(Vec(numLdqEntries, Valid(UInt(log2Ceil(numLdqEntries).W))))

  val ReleaseQueueTail = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)
  val ReleaseQueueHead = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)

  io.release_queue_tail_out := ReleaseQueueTail

  for (w <- 0 until coreWidth) {
    io.load_queue_index_out(w).valid := false.B
    io.load_queue_index_out(w).bits := LoadQueueIndexList(ReleaseQueueTail + w.U)
    when(io.shadow_buffer_head_in > ShadowStampList(ReleaseQueueHead).bits && ShadowStampList(ReleaseQueueHead).valid) {
      io.load_queue_index_out(w).valid := true.B
      io.load_queue_index_out(w).bits := LoadQueueIndexList(ReleaseQueueHead)
      ReleaseQueueHead := (ReleaseQueueHead + 1.U) % numLdqEntries.U
    }
  }

  for (w <- 0 until coreWidth) {
    when(io.load_queue_index_in(w).valid) {
      ShadowStampList(ReleaseQueueTail).valid := true.B
      ShadowStampList(ReleaseQueueTail).bits := io.shadow_buffer_tail_in - 1.U
      LoadQueueIndexList(ReleaseQueueTail) := io.load_queue_index_in(w).bits
      ReleaseQueueTail := (ReleaseQueueTail + 1.U) % numLdqEntries.U
    }
  }

  when(io.br_mispredict_release_queue_idx.valid) {
    ReleaseQueueTail := io.br_mispredict_release_queue_idx.bits

    //TODO: Remove this
    ShadowStampList(io.br_mispredict_release_queue_idx.bits) := 0.U
    LoadQueueIndexList(io.br_mispredict_release_queue_idx.bits) := 0.U
  }

  when(io.flush_in) {
    ReleaseQueueHead := 0.U
    ReleaseQueueTail := 0.U

    //TODO: Remove this
    ShadowStampList(0) := 0.U
    LoadQueueIndexList(0) := 0.U
  }

}
