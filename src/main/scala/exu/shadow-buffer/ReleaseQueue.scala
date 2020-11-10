package boom.exu

import Chisel.{Valid, log2Ceil}
import boom.common.BoomModule
import boom.util.WrapAdd
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

  val index_check = Wire(Bool())
  val same_check = Wire(Bool())

  val ShadowStampList = Reg(Vec(numLdqEntries, Valid(UInt(log2Ceil(maxBrCount).W))))
  val LoadQueueIndexList = Reg(Vec(numLdqEntries, UInt(log2Ceil(numLdqEntries).W)))

  val ReleaseQueueTail = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)
  val ReleaseQueueHead = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)

  io.release_queue_tail_out := ReleaseQueueTail

  dontTouch(index_check)
  dontTouch(same_check)

  var allClear = true.B

  //Use memWidth here, don't need to release more loads than can fire
  for (w <- 0 until memWidth) {
    io.load_queue_index_out(w).valid := false.B
    io.load_queue_index_out(w).bits := LoadQueueIndexList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries))

    index_check := IsIndexBetweenHeadAndTail(ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).bits, io.sb_head, io.sb_tail)
    same_check := ValidAndSame(io.mispredict_new_tail, WrapAdd(ReleaseQueueHead, w.U, numLdqEntries))

    when(allClear && !index_check && !same_check  && ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).valid) {
      io.load_queue_index_out(w).valid := true.B

      ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).valid := false.B
      ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).bits := 0.U

      ReleaseQueueHead := WrapAdd(ReleaseQueueHead, w.U + 1.U, numLdqEntries)
    }.otherwise {
      allClear := false.B
    }
  }

  //This comes from ROB. Can have coreWidth number of new lds.
  var numNewLds = 0.U
  //can have br, ld, br, ld. Track how many sb_inc we have had
  var sb_offset = 0.U
  for (w <- 0 until coreWidth) {
    when(io.new_ldq_idx(w).valid) {
      ShadowStampList(WrapAdd(ReleaseQueueTail, numNewLds, numLdqEntries)).valid := true.B
      ShadowStampList(WrapAdd(ReleaseQueueTail, numNewLds, numLdqEntries)).bits := WrapAdd(io.sb_tail, sb_offset - 1.U, maxBrCount)
      LoadQueueIndexList(WrapAdd(ReleaseQueueTail, numNewLds, numLdqEntries)) := io.new_ldq_idx(w).bits

      numNewLds := numNewLds + 1.U
    }
    //These two should be mutually exclusive
    when(io.new_branch_op(w)) {
      sb_offset = sb_offset + 1.U
    }
    //Increment by number of new entries
    ReleaseQueueTail := WrapAdd(ReleaseQueueTail, numNewLds, numLdqEntries)

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
