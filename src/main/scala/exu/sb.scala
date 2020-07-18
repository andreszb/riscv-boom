package boom.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._
import boom.util._

class ShadowBuffer() extends BoomBundle {

  val io = new Bundle(
    val new_speculative_in = Input(Bool())
  )

  val ShadowBufferHead = RegInit(UInt(8.W), 0.U)
  val ShadowBufferTail = RegInit(UInt(8.W), 0.U)

  val ShadowCaster = Vec(64, Bool())

}

class ReleaseQueue() extends BoomBundle {

  val ShadowStampList = Vec(64, RegInit(UInt(8.W), 0.U))
  val LoadQueueIndexList = Vec(64, RegInit(UInt(8.W), 0.U))
}

class LoadQueue() extends BoomBundle {

  val LoadOpList = Vec(64, RegInit(UInt(8.W), 0.U))
  val IsSpeculativeList = Vec(64, Bool())

}