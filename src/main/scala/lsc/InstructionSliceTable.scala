package lsc

import boom.common.{BoomBundle, BoomModule, MicroOp}
import boom.common._
import boom.exu.{BrResolutionInfo, BusyResp, CommitSignals}
import chisel3._
import chisel3.core.Bundle
import chisel3.util._
import freechips.rocketchip.config.Parameters

class IstCheck (implicit p: Parameters) extends BoomBundle
{
  val tag = Input(ValidIO(UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W)))
  val in_ist = ValidIO(Bool())
}

class IstIO(implicit p: Parameters) extends BoomBundle
{
  val mark = Vec(boomParams.loadSliceCore.get.rdtIstMarkWidth, Input(new IstMark))
  val check = Vec(coreWidth, new IstCheck)
}


class IstMark(implicit p: Parameters) extends BoomBundle
{
  val mark = ValidIO(UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W))
}

abstract class InstructionSliceTable(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends BoomModule {
  val io = IO(new IstIO)
  require(isPow2(entries))
  require(isPow2(ways))
  val lscParams = boomParams.loadSliceCore.get
}

class InstructionSliceTableSyncMem(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends InstructionSliceTable {
  // TODO: Enable bypass from mark->in_ist

  // First the actual Cache with tag, valids and lru
  val tag_table = SyncReadMem(entries, UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W))
  val tag_valids = RegInit(VecInit(Seq.fill(entries)(false.B))) //TODO: Use SyncReadMem
  val tag_lru = RegInit(VecInit(Seq.fill(entries/2)(false.B)))

  // Stage 1
  val ist1_check_valid = Wire(Vec(decodeWidth, Bool()))
  val ist1_check_tag = Wire(Vec(decodeWidth, UInt(lscParams.ibda_tag_sz.W)))
  val ist1_mark_valid  = Wire(Vec(lscParams.rdtIstMarkWidth, Bool()))
  val ist1_mark_tag  = Wire(Vec(lscParams.rdtIstMarkWidth, UInt(lscParams.ibda_tag_sz.W)))

  // Stage 2
  val ist2_check_tag = Reg(Vec(decodeWidth, UInt(lscParams.ibda_tag_sz.W)))
  val ist2_check_valid = Reg(Vec(decodeWidth, Bool()))
  val ist2_check_sram_tag = Wire(Vec(decodeWidth, Vec(ways, UInt(lscParams.ibda_tag_sz.W))))
  val ist2_check_sram_valid = Reg(Vec(decodeWidth, Vec(ways, Bool())))
  val ist2_in_ist = Wire(Vec(decodeWidth, Valid(Bool())))

  require(entries == 128)
  require(ways == 2)
  def index(i: UInt): UInt = {
    val indexBits = log2Up(entries/ways)
    val index = Wire(UInt(indexBits.W))
    if (lscParams.ibdaTagType == IBDA_TAG_FULL_PC) {
      // xor the second lowest bit with the highest index bit so compressed insns are spread around
      index := i(indexBits+2-1, 2) ^ Cat(i(1), 0.U((indexBits-1).W))
    } else if (lscParams.ibdaTagType == IBDA_TAG_INST_LOB) {
      index := Cat(i(12), i(13), i(5,2)) ^ Cat(i(1), 0.U((indexBits-1).W))
      // TODO: Research the entropy in the instruction encoding?
    } else if (lscParams.ibdaTagType == IBDA_TAG_UOPC_LOB) {
      index := i(indexBits+2-1,2) ^ Cat(i(1), 0.U((indexBits-1).W))
    }
    index
  }

  // Stage 1
  for (i <- 0 until decodeWidth) {
    // Connect to IO
    ist1_check_valid(i) := io.check(i).tag.valid
    ist1_check_tag(i) := io.check(i).tag.bits

    // Latch to stage 2
    ist2_check_tag(i) := ist1_check_tag(i)
    ist2_check_valid(i) := ist1_check_valid(i)
  }

  for (i <- 0 until lscParams.rdtIstMarkWidth) {
    ist1_mark_valid(i) := io.mark(i).mark.valid
    ist1_mark_tag(i) := io.mark(i).mark.bits
  }

  // Do a cache read
  for(i <- 0 until decodeWidth) {
    val pc = ist1_check_tag(i)
    val idx = index(pc)
    for (j <- 0 until ways) {
      val tidx = (idx << log2Up(ways)).asUInt() + j.U
      ist2_check_sram_tag(i)(j) := tag_table(tidx)
      ist2_check_sram_valid(i)(j) := tag_valids(tidx)
    }
  }

  // mark - later so mark lrus get priority
  for(i <- 0 until lscParams.rdtIstMarkWidth){
    when(ist1_mark_valid(i)){
      val idx = index(ist1_mark_tag(i))
      tag_lru(idx) := !tag_lru(idx)
      val tidx = (idx << log2Up(ways)).asUInt() + !tag_lru(idx)
      tag_table(tidx) := ist1_mark_tag(i)
      tag_valids(tidx) := true.B

    }
  }

  // Stage 2

  for (i <- 0 until decodeWidth) {
    ist2_in_ist(i).valid := ist2_check_valid(i)
    ist2_in_ist(i).bits := false.B
    val idx = index(ist2_check_tag(i))
    for(j <- 0 until ways) {
      val tidx = (idx << log2Up(ways)).asUInt() + j.U
      when(ist2_check_sram_valid(i)(j) && ist2_check_sram_tag(i)(j) === ist2_check_tag(i)) {
        tag_lru(idx) := j.B // TODO: fix LRU hack
        ist2_in_ist(i).bits := true.B
      }
    }

    io.check(i).in_ist := ist2_in_ist(i)
  }



}


class InstructionSliceTableSyncMem2(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends InstructionSliceTable {

  // First the actual Cache with tag, valids and lru
  val tag_table = SyncReadMem(entries, UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W))
  val tag_valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val tag_lru = RegInit(VecInit(Seq.fill(entries/2)(false.B)))

  // These wires are connected to the cache and reads out the ways for the check req
  val check_cache_idx = Wire(Vec(decodeWidth, Vec(ways, UInt(log2Ceil(entries).W))))
  // signal to MUX out either cache resp or bypass mark from RDT
  val check_select = WireInit(VecInit(Seq.fill(decodeWidth)(0.U(3.W))))

  // Regs to store the last CC mark from RDT
  val mark_last_tag = Reg(Vec(lscParams.rdtIstMarkWidth, UInt(lscParams.ibda_tag_sz.W)))
  val mark_last_valid = Reg(Vec(lscParams.rdtIstMarkWidth,Bool()))

  // Regs to store the last CC check request. Needed for comparison on the cache resp
  val check_req_tag_last = Reg(Vec(decodeWidth, UInt(lscParams.ibda_tag_sz.W)))
  val check_req_valid_last = Reg(Vec(decodeWidth,Bool()))

  // TODO: Remove these
  dontTouch(check_cache_idx)
  dontTouch(check_select)
  dontTouch(mark_last_tag)
  dontTouch(mark_last_valid)
  dontTouch(io.check(0).in_ist)
  dontTouch(io.check(1).in_ist)




  require(entries == 128)
  require(ways == 2)
  def index(i: UInt): UInt = {

    val indexBits = log2Up(entries/ways)
    val index = Wire(UInt(indexBits.W))
    if (lscParams.ibdaTagType == IBDA_TAG_FULL_PC) {
      // xor the second lowest bit with the highest index bit so compressed insns are spread around
      index := i(indexBits+2-1, 2) ^ Cat(i(1), 0.U((indexBits-1).W))
    } else if (lscParams.ibdaTagType == IBDA_TAG_INST_LOB) {
      index := Cat(i(12), i(13), i(5,2)) ^ Cat(i(1), 0.U((indexBits-1).W))
      // TODO: Research the entropy in the instruction encoding?
    } else if (lscParams.ibdaTagType == IBDA_TAG_UOPC_LOB) {
      index := i(indexBits+2-1,2) ^ Cat(i(1), 0.U((indexBits-1).W))
    }

    index

  }
  require(ways == 2, "only one lru bit for now!")


  // check
  for (i <- 0 until coreWidth) {
    // Update the regs latching the check requests
    check_req_valid_last(i) := io.check(i).tag.valid
    check_req_tag_last(i) := io.check(i).tag.bits

    check_select(i) := 1.U // Default to read mem
    val tag = io.check(i).tag.bits
    val idx = index(tag)
    for (j <- 0 until ways) {
      val tidx = (idx << log2Up(ways)).asUInt() + j.U
      check_cache_idx(i)(j) := tidx // Just read out both ways. We do the comparison at the bottom
    }
    // Check if we marked that tag last CC and can do a bypass
    for(w <- 0 until lscParams.rdtIstMarkWidth){
      when(mark_last_valid(w) && mark_last_tag(w) === io.check(i).tag.bits) {
        check_select(i) := 2.U
      }
    }
  }

  // mark - later so mark lrus get priority
  for(i <- 0 until lscParams.rdtIstMarkWidth){
    // Store this mark for next CC to bypass
    mark_last_tag(i) := io.mark(i).mark.bits
    mark_last_valid(i) := io.mark(i).mark.valid

    when(io.mark(i).mark.valid){
      val tag = io.mark(i).mark.bits
      val idx = index(tag)
      val is_match = WireInit(false.B)
      for(j <- 0 until ways){
        val tidx = (idx << log2Up(ways)).asUInt() + j.U
        when(tag_valids(tidx) && tag_table(tidx) === tag){
          tag_lru(idx) := j.B // TODO: fix LRU hack
          is_match := true.B
        }
      }
      when(!is_match){
        tag_lru(idx) := !tag_lru(idx)
        val tidx = (idx << log2Up(ways)).asUInt() + !tag_lru(idx)
        tag_table(tidx) := tag
        tag_valids(tidx) := true.B
      }

      // Can we bypass this mark to check port?
      for (j <- 0 until decodeWidth) {
        when (tag === io.check(j).tag.bits && io.check(j).tag.valid) {
          check_select(j) := 4.U
        }
      }
    }
  }

 // val in_ist = Wire(Vec(decodeWidth, Bool()))
 // dontTouch(in_ist)
  // MUX out check result (1 CC after the lookup)
  for (w <- 0 until decodeWidth) {
    // in_ist signal is only valid if we had a check-lookup the prev cycle
    // TODO: Fix LRU update
    io.check(w).in_ist.valid := check_req_valid_last(w)
    io.check(w).in_ist.bits :=  MuxCase(false.B, Array( check_select(w)(0).asBool() -> check_cache_idx(w).map(idx => tag_table(idx) === check_req_tag_last(w)).reduce(_ || _),
      check_select(w)(1).asBool() -> RegNext(true.B),
      check_select(w)(2).asBool() -> RegNext(true.B))
    )

    /*
    // Mux out the result of the lookup.
    when(check_select(w)(0)) {
      // We have a cache lookup
      val is_match = WireInit(false.B)
      val idx = index(RegNext(io.check(w).tag.bits))
      for (j <- 0 until ways) {
        when(check_req_valid_last(w) && tag_table(check_cache_idx(w)(j)) === check_req_tag_last(w)){ // Do comparison for 2 ways
          tag_lru(idx) := j.B // TODO: fix LRU hack
          is_match := true.B
        }
      }
      in_ist(w) := is_match

    }.otherwise{
      // We have a lookup on a tag that was written the previous cycle or this cycle
      //  Delay a true signal 1 CC
      in_ist(w) := RegNext(true.B)
    }
    io.check(w).in_ist.bits := in_ist(w)
  */
  }


  assert(PopCount(check_select(0)) === 1.U, "[IST] Several bypass-select signals high")
}


class InstructionSliceTableBasic(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends BoomModule{
  val io = IO(new IstIO)
  require(isPow2(entries))
  require(isPow2(ways))

  val tag_table = Reg(Vec(entries, UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W)))
  val tag_valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val tag_lru = RegInit(VecInit(Seq.fill(entries/2)(false.B)))

  val lscParams = boomParams.loadSliceCore.get

  require(entries == 128)
  require(ways == 2)
  def index(i: UInt): UInt = {

    val indexBits = log2Up(entries/ways)
    val index = Wire(UInt(indexBits.W))
    if (lscParams.ibdaTagType == IBDA_TAG_FULL_PC) {
      // xor the second lowest bit with the highest index bit so compressed insns are spread around
      index := i(indexBits+2-1, 2) ^ Cat(i(1), 0.U((indexBits-1).W))
    } else if (lscParams.ibdaTagType == IBDA_TAG_INST_LOB) {
      index := Cat(i(12), i(13), i(5,2)) ^ Cat(i(1), 0.U((indexBits-1).W))
      // TODO: Research the entropy in the instruction encoding?
    } else if (lscParams.ibdaTagType == IBDA_TAG_UOPC_LOB) {
      index := i(indexBits+2-1,2) ^ Cat(i(1), 0.U((indexBits-1).W))
    }

    index

  }
  require(ways == 2, "only one lru bit for now!")
  // check
  for(i <- 0 until coreWidth){
    val pc = io.check(i).tag.bits
    val idx = index(pc)
    val is_match = WireInit(false.B)
    io.check(i).in_ist := is_match // true if pc in IST
    when(io.check(i).tag.valid){
      for(j <- 0 until ways){
        val tidx = (idx << log2Up(ways)).asUInt() + j.U
        when(tag_valids(tidx) && tag_table(tidx) === pc){
          tag_lru(idx) := j.B // TODO: fix LRU hack
          is_match := true.B
        }
      }
    }
  }
  // mark - later so mark lrus get priority
  for(i <- 0 until lscParams.rdtIstMarkWidth){
    when(io.mark(i).mark.valid){
      val pc = io.mark(i).mark.bits
      val idx = index(pc)
      val is_match = WireInit(false.B)
      for(j <- 0 until ways){
        val tidx = (idx << log2Up(ways)).asUInt() + j.U
        when(tag_valids(tidx) && tag_table(tidx) === pc){
          tag_lru(idx) := j.B // TODO: fix LRU hack
          is_match := true.B
        }
      }
      when(!is_match){
        tag_lru(idx) := !tag_lru(idx)
        val tidx = (idx << log2Up(ways)).asUInt() + !tag_lru(idx)
        tag_table(tidx) := pc
        tag_valids(tidx) := true.B
      }
    }
  }
}
