package boom.exu

import Chisel.{MuxCase, PopCount, ShiftRegister, Valid, log2Ceil}
import boom.common.BoomModule
import boom.util.WrapAdd
import chipsalliance.rocketchip.config.Parameters
import chisel3._


class ShadowBuffer(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val flush_in = Input(Bool())
    val sb_tail_reset_idx = Input(Valid(UInt(log2Ceil(maxBrCount).W)))
    val rq_tail = Input(UInt(log2Ceil(numLdqEntries).W))
    val rq_tail_reset_idx = Output(Valid(UInt(log2Ceil(numLdqEntries).W)))

    val new_branch_op = Input(Vec(coreWidth, Bool()))
    val new_ldq_op = Input(Vec(coreWidth, Bool()))
    val br_safe_in = Input(Vec(coreWidth, Valid(UInt(log2Ceil(maxBrCount).W))))

    val sb_head = Output(UInt(log2Ceil(maxBrCount).W))
    val sb_tail = Output(UInt(log2Ceil(maxBrCount).W))
    val sb_empty = Output(Bool())
  }

  def IsIndexBetweenHeadAndTail(Index: UInt, Head: UInt, Tail: UInt): Bool = {
    ((Head < Tail) && Index >= Head && Index < Tail) || ((Head > Tail) && (Index < Tail || Index >= Head))
  }

  //Remember: Head is oldest speculative op, Tail is newest speculative op
  val sb_head = RegInit(UInt(log2Ceil(maxBrCount).W), 0.U)
  val sb_tail = RegInit(UInt(log2Ceil(maxBrCount).W), 0.U)

  val sb = Reg(Vec(maxBrCount, Bool()))
  val rq_reset_idx = Reg(Vec(maxBrCount, UInt(log2Ceil(numLdqEntries).W)))

  val head_is_tail = sb_head === sb_tail
  val sb_empty = PopCount(sb) === 0.U

  val recent_flush = ShiftRegister(io.flush_in, 4)

  io.sb_head := sb_head
  io.sb_tail := sb_tail
  io.sb_empty := sb_empty

  sb_tail := WrapAdd(sb_tail, PopCount(io.new_branch_op), maxBrCount)

  val sb_is_false = Wire(Vec(coreWidth, Bool()))
  val head_is_not_tail = Wire(Vec(coreWidth, Bool()))
  val sb_valid_inc = WireInit(VecInit(Seq.fill(coreWidth)(false.B)))

  for (w <- 0 until coreWidth) {
    sb_is_false(w) := ! sb(WrapAdd(sb_head, w.U, maxBrCount))
  }

  //Increment can only hit head at 0 steps, if empty, so check if it is
  //However, if it is empty, we also need to see if head and tail are aligned
  head_is_not_tail(0) := !head_is_tail || !sb_empty
  sb_valid_inc(0) := (sb_is_false(0) && head_is_not_tail(0))

  for (w <- 1 until coreWidth) {
    head_is_not_tail(w) := WrapAdd(sb_head, w.U, maxBrCount) =/= sb_tail
    sb_valid_inc(w) := (sb_is_false(w) && head_is_not_tail(w)) && sb_valid_inc(w-1)
  }

  val incrementLevel = MuxCase(coreWidth.U, (0 until coreWidth).map(e => sb_valid_inc(e) -> e.U))
  sb_head := WrapAdd(sb_head, PopCount(sb_valid_inc), maxBrCount)

  val br_before = WireInit(VecInit(Seq.fill(coreWidth + 1)(false.B)))
  val masked_ldq = WireInit(VecInit(Seq.fill(coreWidth+1)(0.U(log2Ceil(numLdqEntries).W))))

  for (w <- 0 until coreWidth) {
    when(io.new_branch_op(w) || br_before(w)) {
      br_before(w+1) := true.B
    }
  }
  for (w <- 0 until coreWidth) {
    masked_ldq(w+1) := masked_ldq(w) + (io.new_ldq_op(w) && br_before(w)).asUInt()
  }

  for (w <- 0 until coreWidth) {
    when(io.new_branch_op(w)) {
      sb(WrapAdd(sb_tail, PopCount(io.new_branch_op.slice(0, w)), maxBrCount)) := true.B
      when(!sb_empty) {
        rq_reset_idx(WrapAdd(sb_tail, PopCount(io.new_branch_op.slice(0, w)), maxBrCount)) := WrapAdd(io.rq_tail, PopCount(io.new_ldq_op.slice(0, w)), numLdqEntries)
      }.otherwise {
        rq_reset_idx(WrapAdd(sb_tail, PopCount(io.new_branch_op.slice(0, w)), maxBrCount)) := WrapAdd(io.rq_tail, masked_ldq(w) , numLdqEntries)
      }
    }

    when(io.br_safe_in(w).valid) {
      sb(io.br_safe_in(w).bits) := false.B
    }
  }

  val mispred_rq = RegNext(io.sb_tail_reset_idx.valid && !io.flush_in)
  val mispred_rq_idx = RegNext(rq_reset_idx(io.sb_tail_reset_idx.bits))

  io.rq_tail_reset_idx.valid := false.B
  io.rq_tail_reset_idx.bits := mispred_rq_idx

  when(mispred_rq && !(io.flush_in || recent_flush.asBool())) {
    io.rq_tail_reset_idx.valid := true.B
  }

  //Flush kills all live instructions, so wait 4 cycles before caring about new signals
  when(io.flush_in || recent_flush.asBool()) {
    sb_head := 0.U
    sb_tail := 0.U

    //On a flush, don't send signals to mispredict. Reset instead
    io.rq_tail_reset_idx.valid := false.B

    //TODO: Remove this
    for (w <- 0 until maxBrCount) {
      sb(w) := false.B
      rq_reset_idx(w) := 0.U
    }
  }.elsewhen(io.sb_tail_reset_idx.valid) {
    sb_tail := io.sb_tail_reset_idx.bits

    sb(io.sb_tail_reset_idx.bits) := false.B
    rq_reset_idx(io.sb_tail_reset_idx.bits) := 0.U

    for (w <- 0 until maxBrCount) {
      when(!IsIndexBetweenHeadAndTail(w.U, sb_head, io.sb_tail_reset_idx.bits)) {
        sb(w) := false.B
        rq_reset_idx(w) := 0.U
      }
    }
  }

  assert(!(PopCount(io.new_branch_op) > 0.U && sb_head === sb_tail && !sb_empty), "New entry put into ShadowBuffer while full. Overwriting Valid Entry")
}
