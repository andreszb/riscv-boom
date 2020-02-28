package lsc

import boom.common.{BoomBundle, BoomModule, MicroOp}
import boom.common._
import boom.exu.{BrResolutionInfo, BusyResp, CommitSignals}
import chisel3._
import chisel3.core.Bundle
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.DescribedSRAM

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
  val tag_tables = (0 until ways).map(i => DescribedSRAM(
    name = s"ist_tag_ram_$i",
    desc = "IST Tag Array",
    size = entries/ways,
    data = UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W)
  )._1)
  val tag_valids = (0 until ways).map(_ => RegInit(VecInit(Seq.fill(entries/ways)(false.B)))) //TODO: Use SyncReadMem
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
      ist2_check_sram_tag(i)(j) := tag_tables(j)(idx)
      ist2_check_sram_valid(i)(j) := tag_valids(j)(idx)
    }
  }

  // Stage 2
  for (i <- 0 until decodeWidth) {
    ist2_in_ist(i).valid := ist2_check_valid(i)
    ist2_in_ist(i).bits := false.B
    val idx = index(ist2_check_tag(i))
    for(j <- 0 until ways) {
      when(ist2_check_sram_valid(i)(j) && ist2_check_sram_tag(i)(j) === ist2_check_tag(i)) {
        tag_lru(idx) := j.B // TODO: fix LRU hack
        ist2_in_ist(i).bits := true.B
      }
    }
    io.check(i).in_ist := ist2_in_ist(i)
  }

  // mark - later so mark lrus get priority
  for(i <- 0 until lscParams.rdtIstMarkWidth){
    when(ist1_mark_valid(i)){
      val idx = index(ist1_mark_tag(i))
      when(tag_lru(idx)){
        tag_tables(0)(idx) := ist1_mark_tag(i)
        tag_valids(0)(idx) := true.B
        tag_lru(idx) := false.B
      }.otherwise{
        tag_tables(1)(idx) := ist1_mark_tag(i)
        tag_valids(1)(idx) := true.B
        tag_lru(idx) := true.B
      }
    }
  }
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
