package boom.exu

import Chisel.{Valid, log2Ceil}
import boom.common.BoomModule
import chipsalliance.rocketchip.config.Parameters
import chisel3._

class ReleaseQueue(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val new_ldq_idx = Input(Vec(coreWidth, Valid(UInt(log2Ceil(numLdqEntries).W))))

    val flush_in = Input(Bool())
    val mispredict_new_tail = Input(Valid(UInt(log2Ceil(maxBrCount).W)))

    val sb_head = Input(UInt(log2Ceil(maxBrCount).W))
    val sb_tail = Input(UInt(log2Ceil(maxBrCount).W))

    val load_queue_index_out = Output(Vec(coreWidth, Valid(UInt())))
    val release_queue_tail_out = Output(UInt(log2Ceil(numLdqEntries).W))
  }

  def IsIndexBetweenHeadAndTail(Index: UInt, Head: UInt, Tail: UInt): Bool = {
    ((Head < Tail) && Index >= Head && Index < Tail) || ((Head > Tail) && (Index < Tail || Index >= Head))
  }

  def ValidAndSame(ValidIn: chisel3.util.Valid[UInt], Value: UInt): Bool = {
    ValidIn.valid && ValidIn.bits === Value
  }

  val index_check = Wire(Bool())
  val same_check = Wire(Bool())

  val ShadowStampList = Reg(Vec(numLdqEntries, Valid(UInt(log2Ceil(maxBrCount).W))))
  val LoadQueueIndexList = Reg(Vec(numLdqEntries, UInt(log2Ceil(numLdqEntries).W)))

  val ReleaseQueueTail = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)
  val ReleaseQueueHead = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)

  io.release_queue_tail_out := ReleaseQueueTail

  index_check := IsIndexBetweenHeadAndTail(ShadowStampList(ReleaseQueueHead).bits, io.sb_head, io.sb_tail)
  same_check := ValidAndSame(io.mispredict_new_tail, ReleaseQueueHead)

  dontTouch(index_check)
  dontTouch(same_check)

  for (w <- 0 until coreWidth) {
    io.load_queue_index_out(w).valid := false.B
    io.load_queue_index_out(w).bits := LoadQueueIndexList(ReleaseQueueTail + w.U)
    when(!index_check && !same_check  && ShadowStampList(ReleaseQueueHead).valid) {
      io.load_queue_index_out(w).valid := true.B
      io.load_queue_index_out(w).bits := LoadQueueIndexList(ReleaseQueueHead)

      ShadowStampList(ReleaseQueueHead).valid := false.B
      ShadowStampList(ReleaseQueueHead).bits := 0.U
      ReleaseQueueHead := (ReleaseQueueHead + 1.U) % numLdqEntries.U
    }
  }

  for (w <- 0 until coreWidth) {
    when(io.new_ldq_idx(w).valid) {
      ShadowStampList(ReleaseQueueTail).valid := true.B
      ShadowStampList(ReleaseQueueTail).bits := io.sb_tail - 1.U
      LoadQueueIndexList(ReleaseQueueTail) := io.new_ldq_idx(w).bits
      ReleaseQueueTail := (ReleaseQueueTail + 1.U) % numLdqEntries.U
    }
  }

  val NewTail = Wire(UInt())
  NewTail := io.mispredict_new_tail.bits

  when(io.mispredict_new_tail.valid) {
    ReleaseQueueTail := NewTail
    ShadowStampList(NewTail).valid := false.B

    for (i <- 0 until numLdqEntries) {
      when(!IsIndexBetweenHeadAndTail(i.U, ReleaseQueueHead, NewTail) || NewTail === ReleaseQueueHead) {
        ShadowStampList(i.U).valid := false.B
        ShadowStampList(i.U).bits := 0.U
        LoadQueueIndexList(i.U) := 0.U
      }
    }
  }

  when(io.flush_in) {
    ReleaseQueueHead := 0.U
    ReleaseQueueTail := 0.U
    ShadowStampList(0).valid := false.B

    for (i <- 0 until numLdqEntries) {
      ShadowStampList(i.U).valid := false.B
      ShadowStampList(i.U).bits := 0.U
      LoadQueueIndexList(i.U) := 0.U
    }
  }

}
