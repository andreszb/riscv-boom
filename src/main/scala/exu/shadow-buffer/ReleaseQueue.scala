package boom.exu

import Chisel.{PopCount, Valid, log2Ceil}
import boom.common.BoomModule
import boom.util.{WrapAdd, WrapDec}
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.MuxCase

class ReleaseQueue(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val new_ldq_idx = Input(Vec(coreWidth, Valid(UInt(log2Ceil(numLdqEntries).W))))

    val flush_in = Input(Bool())
    val mispredict_new_tail = Input(Valid(UInt(log2Ceil(numLdqEntries).W)))
    val new_branch_op = Input(Vec(coreWidth, Bool()))

    val sb_head = Input(UInt(log2Ceil(maxBrCount).W))
    val sb_tail = Input(UInt(log2Ceil(maxBrCount).W))
    val sb_full = Input(Bool())
    val sb_empty = Input(Bool())

    val load_queue_index_out = Output(Vec(coreWidth, Valid(UInt())))
    val release_queue_tail_out = Output(UInt(log2Ceil(numLdqEntries).W))
  }

  val index_check = Wire(Vec(coreWidth, Bool()))
  val same_check = Wire(Vec(coreWidth, Bool()))
  val all_valid = Wire(Vec(coreWidth, Bool()))
  val sb_full_last_cycle = RegNext(io.sb_full)

  val ShadowStampList = Reg(Vec(numLdqEntries, Valid(UInt(log2Ceil(maxBrCount).W))))
  val LoadQueueIndexList = Reg(Vec(numLdqEntries, UInt(log2Ceil(numLdqEntries).W)))

  val ReleaseQueueTail = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)
  val ReleaseQueueHead = RegInit(UInt(log2Ceil(numLdqEntries).W), 0.U)

  io.release_queue_tail_out := ReleaseQueueTail

  def IsIndexBetweenHeadAndTail(Index: UInt, Head: UInt, Tail: UInt): Bool = {
    ((Head < Tail) && Index >= Head && Index < Tail) || ((Head > Tail) && (Index < Tail || Index >= Head)) || (Head === Tail && (io.sb_full || sb_full_last_cycle))
  }

  def ValidAndSame(ValidIn: chisel3.util.Valid[UInt], Value: UInt): Bool = {
    ValidIn.valid && ValidIn.bits === Value
  }


  dontTouch(index_check)
  dontTouch(same_check)
  dontTouch(io.load_queue_index_out)

  for (w <- 0 until coreWidth) {
    index_check(w) := IsIndexBetweenHeadAndTail(ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).bits, io.sb_head, io.sb_tail)
    same_check(w) := ValidAndSame(io.mispredict_new_tail, WrapAdd(ReleaseQueueHead, w.U, numLdqEntries))
  }
  all_valid(0) := ShadowStampList(ReleaseQueueHead).valid
  for (w <- 1 until coreWidth) {
    all_valid(w) := all_valid(w-1) && ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).valid
  }


  //Release as fast as new ones can enter
  for (w <- 0 until coreWidth) {
    io.load_queue_index_out(w).valid := false.B
    io.load_queue_index_out(w).bits := LoadQueueIndexList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries))

    //All current entries for the checks needs to be 0
    when(PopCount(index_check.slice(0, w+1)) === 0.U
      && PopCount(same_check.slice(0, w+1)) === 0.U
      && all_valid(w)) {
      io.load_queue_index_out(w).valid := true.B

      ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).valid := false.B
      ShadowStampList(WrapAdd(ReleaseQueueHead, w.U, numLdqEntries)).bits := 0.U
    }
  }

  //ReleaseQueueHead should be incremented by the amount of freed loads
  ReleaseQueueHead := WrapAdd(ReleaseQueueHead, PopCount(io.load_queue_index_out.map(e => e.valid)), numLdqEntries)

  //This comes from ROB. Can have coreWidth number of new lds.
  //can have br, ld, br, ld. Track how many sb_inc we have had

  val branch_before = WireInit(VecInit(Seq.fill(coreWidth + 1)(false.B)))
  val masked_ldq = WireInit(VecInit(Seq.fill(coreWidth+1)(0.U(log2Ceil(numLdqEntries).W))))

  for (w <- 0 until coreWidth) {
    when(io.new_branch_op(w) || branch_before(w)) {
      branch_before(w+1) := true.B
    }
  }
  for (w <- 0 until coreWidth) {
    masked_ldq(w+1) := masked_ldq(w) + (io.new_ldq_idx(w).valid && branch_before(w)).asUInt()
  }

  val sb_branch_offset = Wire(Vec(coreWidth, UInt(log2Ceil(numLdqEntries).W)))
  val rq_load_offset = Wire(Vec(coreWidth, UInt(log2Ceil(numLdqEntries).W)))

  for (w <- 0 until coreWidth) {
    sb_branch_offset(w) := WrapDec(WrapAdd(io.sb_tail, PopCount(io.new_branch_op.slice(0, w)), maxBrCount), maxBrCount)
    rq_load_offset(w) := WrapAdd(ReleaseQueueTail, PopCount(io.new_ldq_idx.slice(0, w).map(_.valid)), numLdqEntries)
  }

  for (w <- 0 until coreWidth) {
    when(!io.sb_empty && io.new_ldq_idx(w).valid) {
      ShadowStampList(rq_load_offset(w)).bits := sb_branch_offset(w)
      ShadowStampList(rq_load_offset(w)).valid := true.B
      LoadQueueIndexList(rq_load_offset(w)) := io.new_ldq_idx(w).bits
    }.elsewhen(io.new_ldq_idx(w).valid && branch_before(w)) {
      ShadowStampList(WrapAdd(ReleaseQueueTail, masked_ldq(w), numLdqEntries)).bits := sb_branch_offset(w)
      ShadowStampList(WrapAdd(ReleaseQueueTail, masked_ldq(w), numLdqEntries)).valid := true.B
      LoadQueueIndexList(rq_load_offset(w)) := io.new_ldq_idx(w).bits
    }
    assert(!(io.new_ldq_idx(w).valid && io.new_branch_op(w)))
  }

  //ReleaseQueueTail incremented by number of loads when in shadow mode, or loads after branch if not
  when((io.sb_tail =/= io.sb_head) || io.sb_full) {
    ReleaseQueueTail := WrapAdd(ReleaseQueueTail, PopCount(io.new_ldq_idx.map(_.valid)), numLdqEntries)
  }.otherwise{
    ReleaseQueueTail := WrapAdd(ReleaseQueueTail, masked_ldq(coreWidth), numLdqEntries)
  }

  dontTouch(branch_before)
  dontTouch(masked_ldq)
  dontTouch(sb_branch_offset)
  dontTouch(rq_load_offset)


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
