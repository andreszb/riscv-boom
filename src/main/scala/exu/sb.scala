package boom.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._
import boom.util._


//TODO: Finish this one
class ShadowBuffer(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val new_speculative_in = Input(Bool())
    val load_op = Input(new MicroOp())

    val speculative_commited = Input(Bool())

    val shadow_buffer_head_in = Input(UInt(8.W))
    val shadow_buffer_tail_in = Input(UInt(8.W))

    val shadow_buffer_head_out = Output(UInt(8.W))
    val shadow_buffer_tail_out = Output(UInt(8.W))


  }

  val ShadowBufferHead = RegInit(UInt(8.W), 0.U)
  val ShadowBufferTail = RegInit(UInt(8.W), 0.U)

  val ShadowCaster = Vec(64, Bool())

  ShadowBufferHead := io.shadow_buffer_head_in
  ShadowBufferTail := io.shadow_buffer_tail_in

  io.shadow_buffer_head_out := ShadowBufferHead
  io.shadow_buffer_tail_out := ShadowBufferTail

  when(io.new_speculative_in > ShadowBufferTail){

  }

  val LoadQueue = Module(new LoadQueue())
  val ReleaseQueue = Module(new ReleaseQueue())


}
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
//This one is done
class LoadQueue(implicit p: Parameters) extends BoomModule {

  val io = new Bundle{
    val is_new_load = Input(Bool())
    val new_load_op = Input(new MicroOp())
    val is_speculative = Input(Bool())

    val load_queue_index_head = Input(UInt())
  }

  val LoadOpList = Vec(64, RegInit(new MicroOp(), NullMicroOp()))
  val IsSpeculativeList = Vec(64, Bool())

  val LoadHead = RegInit(UInt(8.W), 0.U)
  val LoadTail = RegInit(UInt(8.W), 0.U)

  when(io.is_new_load) {
    LoadHead := (LoadHead + 1.U) % 64.U
    LoadOpList(LoadHead) := io.new_load_op
    IsSpeculativeList(LoadHead) := io.is_speculative
  }

  when (io.load_queue_index_head > LoadTail) {
    LoadTail := (LoadTail + 1.U) % 64.U
  }
}