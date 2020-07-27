package boom.exu

import boom.common.{BoomModule, MicroOp, NullMicroOp}
import chipsalliance.rocketchip.config.Parameters
import chisel3._

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
    LoadOpList(LoadHead) := io.new_load_op
    IsSpeculativeList(LoadHead) := io.is_speculative
    LoadHead := (LoadHead + 1.U) % 64.U
  }

  when (io.load_queue_index_head > LoadTail) {
    LoadTail := (LoadTail + 1.U) % 64.U
  }
}
