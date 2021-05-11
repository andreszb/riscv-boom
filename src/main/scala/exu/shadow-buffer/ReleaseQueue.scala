package boom.exu

import Chisel.{PopCount, ShiftRegister, Valid, log2Ceil}
import boom.common.BoomModule
import boom.util.{WrapAdd, WrapDec}
import chipsalliance.rocketchip.config.Parameters
import chisel3._

class ReleaseQueue(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val new_branch_op = Input(Vec(coreWidth, Bool()))
    val new_ldq_idx = Input(Vec(coreWidth, Valid(UInt(log2Ceil(numLdqEntries).W))))

    val flush_in = Input(Bool())
    val rq_tail_reset_idx = Input(Valid(UInt(log2Ceil(numRqEntries).W)))

    val sb_head = Input(UInt(log2Ceil(numSbEntries).W))
    val sb_tail = Input(UInt(log2Ceil(numSbEntries).W))
    val sb_empty = Input(Bool())

    val lq_index = Output(Vec(coreWidth, Valid(UInt())))
    val rq_tail = Output(UInt(log2Ceil(numRqEntries).W))
    val leading_shadow_tag = Output(Valid(UInt(log2Ceil(numRqEntries).W)))
  }

  val index_check = Wire(Vec(coreWidth, Bool()))
  val same_check = Wire(Vec(coreWidth, Bool()))
  val all_valid = Wire(Vec(coreWidth, Bool()))

  val shadow_tag_list = Reg(Vec(numRqEntries, Valid(UInt(log2Ceil(numSbEntries).W))))
  val ldq_idx_list = Reg(Vec(numRqEntries, UInt(log2Ceil(numLdqEntries).W)))

  val rq_tail = RegInit(UInt(log2Ceil(numRqEntries).W), 0.U)
  val rq_head = RegInit(UInt(log2Ceil(numRqEntries).W), 0.U)

  val recent_flush = ShiftRegister(io.flush_in, 4)

  io.rq_tail := rq_tail
  io.leading_shadow_tag.valid := shadow_tag_list(WrapAdd(rq_head, (2*coreWidth).U, numRqEntries)).valid
  io.leading_shadow_tag.bits := shadow_tag_list(WrapAdd(rq_head, (2*coreWidth).U, numRqEntries)).bits

  def IsIndexBetweenHeadAndTail(Index: UInt, Head: UInt, Tail: UInt): Bool = {
    ((Head < Tail) && Index >= Head && Index < Tail) || ((Head > Tail) && (Index < Tail || Index >= Head)) || (Head === Tail && !io.sb_empty)
  }

  def ValidAndSame(ValidIn: chisel3.util.Valid[UInt], Value: UInt): Bool = {
    ValidIn.valid && ValidIn.bits === Value
  }


  for (w <- 0 until coreWidth) {
    index_check(w) := IsIndexBetweenHeadAndTail(shadow_tag_list(WrapAdd(rq_head, w.U, numRqEntries)).bits, io.sb_head, io.sb_tail)
    same_check(w) := ValidAndSame(io.rq_tail_reset_idx, WrapAdd(rq_head, w.U, numRqEntries))
  }
  all_valid(0) := shadow_tag_list(rq_head).valid
  for (w <- 1 until coreWidth) {
    all_valid(w) := all_valid(w-1) && shadow_tag_list(WrapAdd(rq_head, w.U, numRqEntries)).valid
  }


  //Release as fast as new ones can enter
  for (w <- 0 until coreWidth) {
    io.lq_index(w).valid := false.B
    io.lq_index(w).bits := ldq_idx_list(WrapAdd(rq_head, w.U, numRqEntries))

    //All current entries for the checks needs to be 0
    when(PopCount(index_check.slice(0, w+1)) === 0.U
      && PopCount(same_check.slice(0, w+1)) === 0.U
      && all_valid(w)) {
      io.lq_index(w).valid := true.B

      shadow_tag_list(WrapAdd(rq_head, w.U, numRqEntries)).valid := false.B
      shadow_tag_list(WrapAdd(rq_head, w.U, numRqEntries)).bits := 0.U
    }
  }

  //ReleaseQueueHead should be incremented by the amount of freed loads
  rq_head := WrapAdd(rq_head, PopCount(io.lq_index.map(e => e.valid)), numRqEntries)

  //This comes from ROB. Can have coreWidth number of new lds or brs.
  //Can come in any order. Need count of lds after branch
  val branch_before = WireInit(VecInit(Seq.fill(coreWidth + 1)(false.B)))
  val masked_ldq = WireInit(VecInit(Seq.fill(coreWidth+1)(0.U(log2Ceil(numRqEntries).W))))

  for (w <- 0 until coreWidth) {
    when(io.new_branch_op(w) || branch_before(w)) {
      branch_before(w+1) := true.B
    }
  }
  for (w <- 0 until coreWidth) {
    masked_ldq(w+1) := masked_ldq(w) + (io.new_ldq_idx(w).valid && branch_before(w)).asUInt()
  }

  val sb_branch_offset = Wire(Vec(coreWidth, UInt(log2Ceil(numRqEntries).W)))
  val rq_load_offset = Wire(Vec(coreWidth, UInt(log2Ceil(numRqEntries).W)))

  for (w <- 0 until coreWidth) {
    sb_branch_offset(w) := WrapDec(WrapAdd(io.sb_tail, PopCount(io.new_branch_op.slice(0, w)), numSbEntries), numSbEntries)
    rq_load_offset(w) := WrapAdd(rq_tail, PopCount(io.new_ldq_idx.slice(0, w).map(_.valid)), numRqEntries)
  }

  for (w <- 0 until coreWidth) {
    when(!io.sb_empty && io.new_ldq_idx(w).valid) {
      shadow_tag_list(rq_load_offset(w)).bits := sb_branch_offset(w)
      shadow_tag_list(rq_load_offset(w)).valid := true.B
      ldq_idx_list(rq_load_offset(w)) := io.new_ldq_idx(w).bits
    }.elsewhen(io.new_ldq_idx(w).valid && branch_before(w)) {
      shadow_tag_list(WrapAdd(rq_tail, masked_ldq(w), numRqEntries)).bits := sb_branch_offset(w)
      shadow_tag_list(WrapAdd(rq_tail, masked_ldq(w), numRqEntries)).valid := true.B
      ldq_idx_list(WrapAdd(rq_tail, masked_ldq(w), numRqEntries)) := io.new_ldq_idx(w).bits
    }
    assert(!(io.new_ldq_idx(w).valid && io.new_branch_op(w)))
  }

  //ReleaseQueueTail incremented by number of loads when in shadow mode, or loads after branch if not
  when(!io.sb_empty) {
    rq_tail := WrapAdd(rq_tail, PopCount(io.new_ldq_idx.map(_.valid)), numRqEntries)
  }.otherwise{
    rq_tail := WrapAdd(rq_tail, masked_ldq(coreWidth), numRqEntries)
  }

  val NewTail = Wire(UInt())
  NewTail := io.rq_tail_reset_idx.bits

  when(io.rq_tail_reset_idx.valid) {
    rq_tail := NewTail
    shadow_tag_list(NewTail).valid := false.B

    for (i <- 0 until numRqEntries) {
      when(!IsIndexBetweenHeadAndTail(i.U, rq_head, NewTail) || NewTail === rq_head) {
        shadow_tag_list(i.U).valid := false.B
        shadow_tag_list(i.U).bits := 0.U
        ldq_idx_list(i.U) := 0.U
      }
    }
  }

  //Reset on a flush and ignore signals for 4 cycles
  when(io.flush_in || recent_flush.orR()) {
    rq_head := 0.U
    rq_tail := 0.U
    shadow_tag_list(0).valid := false.B

    for (i <- 0 until numRqEntries) {
      shadow_tag_list(i.U).valid := false.B
      shadow_tag_list(i.U).bits := 0.U
      ldq_idx_list(i.U) := 0.U
    }
  }

  assert(!(PopCount(shadow_tag_list.map(e => e.valid)) === 0.U && rq_head =/= rq_tail), "Empty ReleaseQueue, yet head and tail disjointed")

}
