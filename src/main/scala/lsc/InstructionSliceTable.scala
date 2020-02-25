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
  val in_ist = Output(Bool())
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


class InstructionSliceTableSyncMem(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends BoomModule{
  val io = IO(new IstIO)
  require(isPow2(entries))
  require(isPow2(ways))

  val tag_table = Reg(Vec(entries, UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W)))
  val tag_valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val tag_lru = RegInit(VecInit(Seq.fill(entries/2)(false.B)))

  val lscParams = boomParams.loadSliceCore.get


  val check_mem_idx = WireInit(VecInit(Seq.fill(decodeWidth)(VecInit(Seq.fill(ways)(0.U(log2Ceil(entries).W))))))
  val mark_last = WireInit(VecInit(Seq.fill(decodeWidth)(0.U(lscParams.ibda_tag_sz().W))))
  val mark_curr = WireInit(VecInit(Seq.fill(decodeWidth)(0.U(lscParams.ibda_tag_sz().W))))
  val select = WireInit(VecInit(Seq.fill(decodeWidth)(0.U(3.W))))

  val mark_last_tag = Reg(Vec(lscParams.rdtIstMarkWidth, UInt(lscParams.ibda_tag_sz.W)))
  val mark_last_valid = Reg(Vec(lscParams.rdtIstMarkWidth,Bool()))


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
    select(i) := 1.U
    val tag = io.check(i).tag.bits
    val idx = index(tag)
    when(io.check(i).tag.valid){
      for(j <- 0 until ways){
        val tidx = (idx << log2Up(ways)).asUInt() + j.U
        check_mem_idx(i)(j) := tidx
      }

      // Check if we marked that tag last CC and can do a bypass
      for(w <- 0 until lscParams.rdtIstMarkWidth){
        when(mark_last_valid(w) && mark_last_tag(w) === io.check(i).tag.bits) {
          select := 2.U
          mark_last(i) := mark_last_tag(w)
        }
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
          select := 4.U
          mark_curr(j) := tag
        }
      }
    }
  }

  // Route out check result (1 CC after the lookup)
  for (w <- 0 until decodeWidth) {
    io.check(w).in_ist  := MuxCase(0.U, Array(  select(w)(0).asBool() -> check_mem_idx(w).map(idx => tag_table(idx) === io.check(w).tag.bits).reduce(_||_),
                                                select(w)(1).asBool() -> RegNext(mark_last(w) === io.check(w).tag.bits),
                                                select(w)(2).asBool() -> RegNext(mark_curr(w) === io.check(w).tag.bits))
    )
  }

  assert(PopCount(select(0)) === 1.U, "[IST] Several bypass-select signals high")
}


class InstructionSliceTable(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends BoomModule{
  val io = IO(new IstIO)
  require(isPow2(entries))
  require(isPow2(ways))

  val tag_table = SyncReadMem(entries, UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W))
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
