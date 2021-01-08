package boom.exu

import Chisel.{PopCount, Valid, log2Ceil}
import boom.common.BoomModule
import boom.util.{WrapAdd, WrapDec}
import chipsalliance.rocketchip.config.Parameters
import chisel3._

class ReleaseQueue(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val new_ldq_idx = Input(Vec(coreWidth, Valid(UInt(log2Ceil(numLdqEntries).W))))

    val flush_in = Input(Bool())
    val mispredict_new_tail = Input(Valid(UInt(log2Ceil(maxBrCount).W)))
    val new_branch_op = Input(Vec(coreWidth, Bool()))

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

  val index_check = Wire(Vec(coreWidth, Bool()))
  val same_check = Wire(Vec(coreWidth, Bool()))

  val ShadowStampList = Reg(Vec(numLdqEntries, Valid(UInt(log2Ceil(maxBrCount).W))))
  val LoadQueueIndexList = Reg(Vec(numLdqEntries, UInt(log2Ceil(numLdqEntries).W)))

  val ReleaseQueueTail = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)
  val ReleaseQueueHead = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)

  io.release_queue_tail_out := ReleaseQueueTail

  dontTouch(index_check)
  dontTouch(same_check)
  dontTouch(io.load_queue_index_out)


  //Release as fast as new ones can enter
  for (w <- 0 until coreWidth) {
    io.load_queue_index_out(w).valid := false.B
    io.load_queue_index_out(w).bits := LoadQueueIndexList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries))

    index_check(w) := IsIndexBetweenHeadAndTail(ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).bits, io.sb_head, io.sb_tail)
    same_check(w) := ValidAndSame(io.mispredict_new_tail, WrapAdd(ReleaseQueueHead, w.U, numLdqEntries))

    //All current entries for the checks needs to be 0
    when(PopCount(index_check.slice(0, w+1)) === 0.U
      && PopCount(same_check.slice(0, w+1)) === 0.U
      && ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).valid) {
      io.load_queue_index_out(w).valid := true.B

      ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).valid := false.B
      ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).bits := 0.U
    }
  }

  //ReleaseQueueHead should be incremented by the amount of freed loads
  ReleaseQueueHead := WrapAdd(ReleaseQueueHead, PopCount(io.load_queue_index_out.map(e => e.valid)), numLdqEntries)

  //This comes from ROB. Can have coreWidth number of new lds.
  //can have br, ld, br, ld. Track how many sb_inc we have had

  for (w <- 0 until coreWidth) {
    when(io.new_ldq_idx(w).valid && (io.sb_head =/= io.sb_tail || PopCount(io.new_branch_op.slice(0,w)) > 0.U)) {
      val LoadOffset = Wire(UInt())
      val TailOffset = Wire(UInt())
      LoadOffset := PopCount(io.new_ldq_idx.slice(0, w).map(e => e.valid))
      TailOffset := PopCount(io.new_branch_op.slice(0, w))
      ShadowStampList(WrapAdd(ReleaseQueueTail, LoadOffset, numLdqEntries)).valid := true.B
      ShadowStampList(WrapAdd(ReleaseQueueTail, LoadOffset, numLdqEntries)).bits := WrapDec(WrapAdd(io.sb_tail, TailOffset, maxBrCount), maxBrCount)
      LoadQueueIndexList(WrapAdd(ReleaseQueueTail, LoadOffset, numLdqEntries)) := io.new_ldq_idx(w).bits
    }

    when(io.sb_head =/= io.sb_tail ||
      (io.new_ldq_idx.slice(0, w+1).map(e => e.valid).contains(true.B).B && io.new_branch_op.slice(0, w).contains(true.B).B)) {
      ReleaseQueueTail := WrapAdd(ReleaseQueueTail, PopCount(io.new_ldq_idx.slice(0, w+1).map(e => e.valid)), numLdqEntries)
    }

    assert(!(io.new_ldq_idx(w).valid && io.new_branch_op(w)))
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
