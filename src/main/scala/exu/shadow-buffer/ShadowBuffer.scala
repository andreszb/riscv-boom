package boom.exu

import Chisel.{MuxCase, MuxLookup, PopCount, PriorityMux, Valid, log2Ceil}
import boom.common.BoomModule
import boom.util.{WrapAdd, WrapDec}
import chipsalliance.rocketchip.config.Parameters
import chisel3._

import java.util.stream.IntStream

class ShadowBuffer(implicit p: Parameters) extends BoomModule {

  val io = new Bundle {
    val flush_in = Input(Bool())
    val br_mispred_shadow_buffer_idx = Input(Valid(UInt(log2Ceil(maxBrCount).W)))
    val release_queue_tail_checkpoint = Input(UInt(log2Ceil(numLdqEntries).W))
    val br_mispredict_release_queue_idx = Output(Valid(UInt(log2Ceil(numLdqEntries).W)))

    val new_branch_op = Input(Vec(coreWidth, Bool()))
    val new_ldq_op = Input(Vec(coreWidth, Bool()))
    val br_safe_in = Input(Vec(coreWidth, Valid(UInt(log2Ceil(maxBrCount).W))))

    val shadow_buffer_head_out = Output(UInt(log2Ceil(maxBrCount).W))
    val shadow_buffer_tail_out = Output(UInt(log2Ceil(maxBrCount).W))
    val shadow_buffer_full_out = Output(Bool())
    val shadow_buffer_empty_out = Output(Bool())
  }

  def IsIndexBetweenHeadAndTail(Index: UInt, Head: UInt, Tail: UInt): Bool = {
    ((Head < Tail) && Index >= Head && Index < Tail) || ((Head > Tail) && (Index < Tail || Index >= Head))
  }

  //Remember: Head is oldest speculative op, Tail is newest speculative op
  val ShadowBufferHead = RegInit(UInt(log2Ceil(maxBrCount).W), 0.U)
  val ShadowBufferTail = RegInit(UInt(log2Ceil(maxBrCount).W), 0.U)

  val ShadowCaster = Reg(Vec(maxBrCount, Bool()))
  val ReleaseQueueIndex = Reg(Vec(maxBrCount, UInt(log2Ceil(numLdqEntries).W)))

  val update_release_queue = RegNext(io.br_mispred_shadow_buffer_idx.valid && !io.flush_in)
  val update_release_queue_idx = RegNext(ReleaseQueueIndex(io.br_mispred_shadow_buffer_idx.bits))
  //TODO: Add HEAD==TAIL wire

  io.shadow_buffer_head_out := ShadowBufferHead
  io.shadow_buffer_tail_out := ShadowBufferTail
  io.shadow_buffer_full_out := PopCount(ShadowCaster) === maxBrCount.U
  io.shadow_buffer_empty_out := PopCount(ShadowCaster) === 0.U

  ShadowBufferTail := WrapAdd(ShadowBufferTail, PopCount(io.new_branch_op), maxBrCount)

  val ShadowCasterIsFalse = Wire(Vec(coreWidth, Bool()))
  val HeadIsNotTail = Wire(Vec(coreWidth, Bool()))
  val ShadowCasterValidIncrement = WireInit(VecInit(Seq.fill(coreWidth)(false.B)))
  val ShadowBufferFullLastCycle = RegNext(ShadowBufferHead === ShadowBufferTail && ShadowCaster(ShadowBufferHead))

  for (w <- 0 until coreWidth) {
    ShadowCasterIsFalse(w) := ! ShadowCaster(WrapAdd(ShadowBufferHead, w.U, maxBrCount))
  }

  //Increment can only hit head at 0 steps, if empty, so check if it is
  //However, if it is empty, we also need to see if head and tail are aligned
  HeadIsNotTail(0) := ShadowBufferHead =/= ShadowBufferTail || PopCount(ShadowCaster) =/= 0.U
  ShadowCasterValidIncrement(0) := (ShadowCasterIsFalse(0) && HeadIsNotTail(0))
  for (w <- 1 until coreWidth) {
    HeadIsNotTail(w) := WrapAdd(ShadowBufferHead, w.U, maxBrCount) =/= ShadowBufferTail
    ShadowCasterValidIncrement(w) := (ShadowCasterIsFalse(w) && HeadIsNotTail(w)) && ShadowCasterValidIncrement(w-1)
  }
  val incrementLevel = MuxCase(coreWidth.U, (0 until coreWidth).map(e => ShadowCasterValidIncrement(e) -> e.U))
  ShadowBufferHead := WrapAdd(ShadowBufferHead, PopCount(ShadowCasterValidIncrement), maxBrCount)

  val branch_before = WireInit(VecInit(Seq.fill(coreWidth + 1)(false.B)))
  val masked_ldq = WireInit(VecInit(Seq.fill(coreWidth+1)(0.U(log2Ceil(numLdqEntries).W))))

  for (w <- 0 until coreWidth) {
    when(io.new_branch_op(w) || branch_before(w)) {
      branch_before(w+1) := true.B
    }
  }
  for (w <- 0 until coreWidth) {
    masked_ldq(w+1) := masked_ldq(w) + (io.new_ldq_op(w) && branch_before(w)).asUInt()
  }

  for (w <- 0 until coreWidth) {
    when(io.new_branch_op(w)) {
      ShadowCaster(WrapAdd(ShadowBufferTail, PopCount(io.new_branch_op.slice(0, w)), maxBrCount)) := true.B
      when(!io.shadow_buffer_empty_out) {
        ReleaseQueueIndex(WrapAdd(ShadowBufferTail, PopCount(io.new_branch_op.slice(0, w)), maxBrCount)) := WrapAdd(io.release_queue_tail_checkpoint, PopCount(io.new_ldq_op.slice(0, w)), numLdqEntries)
      }.otherwise {
        ReleaseQueueIndex(WrapAdd(ShadowBufferTail, PopCount(io.new_branch_op.slice(0, w)), maxBrCount)) := WrapAdd(io.release_queue_tail_checkpoint, masked_ldq(w) , numLdqEntries)
      }
    }

    when(io.br_safe_in(w).valid) {
      ShadowCaster(io.br_safe_in(w).bits) := false.B
    }
  }

  io.br_mispredict_release_queue_idx.valid := false.B
  io.br_mispredict_release_queue_idx.bits := update_release_queue_idx

  when(update_release_queue) {
    io.br_mispredict_release_queue_idx.valid := true.B
  }

  //Flush has at least a 2 cycle penalty before execution stops. TODO: Check this later
  when(io.flush_in || RegNext(io.flush_in)) {
    ShadowBufferHead := 0.U
    ShadowBufferTail := 0.U

    //On a flush, don't send signals to mispredict. Reset instead
    io.br_mispredict_release_queue_idx.valid := false.B

    //TODO: Remove this
    for (w <- 0 until maxBrCount) {
      ShadowCaster(w) := false.B
      ReleaseQueueIndex(w) := 0.U
    }
  }.elsewhen(io.br_mispred_shadow_buffer_idx.valid) {
    ShadowBufferTail := io.br_mispred_shadow_buffer_idx.bits

    ShadowCaster(io.br_mispred_shadow_buffer_idx.bits) := false.B
    ReleaseQueueIndex(io.br_mispred_shadow_buffer_idx.bits) := 0.U

    for (w <- 0 until maxBrCount) {
      when(!IsIndexBetweenHeadAndTail(w.U, ShadowBufferHead, io.br_mispred_shadow_buffer_idx.bits)) {
        ShadowCaster(w) := false.B
        ReleaseQueueIndex(w) := 0.U
      }
    }
  }

  assert(!(PopCount(io.new_branch_op) > 0.U && ShadowBufferHead === ShadowBufferTail && !io.shadow_buffer_empty_out), "New entry put into ShadowBuffer while full. Overwriting Valid Entry")
}
