package boom.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._
import boom.util._

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

class ReleaseQueue(implicit p: Parameters) extends BoomModule {

  val ShadowStampList = Vec(64, RegInit(UInt(8.W), 0.U))
  val LoadQueueIndexList = Vec(64, RegInit(UInt(8.W), 0.U))

  override def io: Record = ???
}

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