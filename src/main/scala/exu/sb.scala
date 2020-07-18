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

  when(io.shadow_buffer_tail_in > ShadowBufferTail){

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

  val LoadOpList = Vec(64, RegInit(UInt(8.W), 0.U))
  val IsSpeculativeList = Vec(64, Bool())

  override def io: Record = ???
}