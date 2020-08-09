package boom.exu

import boom.common.{BoomModule, MicroOp, NullMicroOp}
import chipsalliance.rocketchip.config.Parameters
import chisel3._

//This one is done
class LoadQueue(implicit p: Parameters) extends BoomModule {

  val io = new Bundle{
    val is_new_load = Input(Vec(coreWidth, Bool()))
    val new_load_op = Input(Vec(coreWidth, new MicroOp()))
    val is_speculative = Input(Vec(coreWidth, Bool()))

    val load_queue_index_head = Input(UInt())
  }

  val LoadOpList = Reg(Vec(64, RegInit(new MicroOp(), NullMicroOp())))
  val IsSpeculativeList = Reg(Vec(64, Bool()))

  val LoadHead = RegInit(UInt(8.W), 0.U)
  val LoadTail = RegInit(UInt(8.W), 0.U)

  for (w <- 0 until coreWidth) {
    when(io.is_new_load(w)) {
      LoadOpList(LoadHead) := io.new_load_op(w)
      IsSpeculativeList(LoadHead) := io.is_speculative(w)
      LoadHead := (LoadHead + 1.U) % 64.U
    }
  }

  for (w <- 0 until coreWidth) {
    when (io.load_queue_index_head > LoadTail) {
      LoadTail := (LoadTail + 1.U) % 64.U
    }
  }
}
