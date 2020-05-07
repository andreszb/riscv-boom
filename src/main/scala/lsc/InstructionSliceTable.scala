package lsc

import boom.common.{BoomBundle, BoomModule, MicroOp}
import boom.common._
import boom.exu.{BrResolutionInfo, BusyResp, CommitSignals}
import chisel3._
import chisel3.core.Bundle
import chisel3.internal.naming.chiselName
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.DescribedSRAM

class IstCheck (implicit p: Parameters) extends BoomBundle
{
  val tag = Input(ValidIO(UInt(boomParams.ibdaParams.get.ibda_tag_sz.W)))
  val in_ist = ValidIO(Bool())
}

class IstIO(implicit p: Parameters) extends BoomBundle
{
  val mark = Vec(boomParams.ibdaParams.get.rdtIstMarkWidth, Input(new IstMark))
  val check = Vec(coreWidth, new IstCheck)
}


class IstMark(implicit p: Parameters) extends BoomBundle
{
  val mark = ValidIO(UInt(boomParams.ibdaParams.get.ibda_tag_sz.W))
}

abstract class InstructionSliceTable(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends BoomModule {
  val io = IO(new IstIO)
  require(isPow2(entries))
  require(isPow2(ways))
  val ibdaParams = boomParams.ibdaParams.get
}

class InstructionSliceTableBloom(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends InstructionSliceTable {
  val bloom = Module(new BloomFilterModified(m = 2048, k = 6, inBits = boomParams.ibdaParams.get.ibda_tag_sz, reads = 2, collisionRate = 0.0001))
  require(ibdaParams.rdtIstMarkWidth == 1)

  bloom.io.insert.valid := io.mark(0).mark.valid
  bloom.io.insert.bits := io.mark(0).mark.bits
  for (i <- 0 until decodeWidth) {
    bloom.io.test(i).bits := io.check(i).tag.bits
    io.check(i).in_ist.bits := bloom.io.test(i).found
    io.check(i).in_ist.valid := RegNext(io.check(i).tag.valid)
  }
}

@chiselName
class InstructionSliceTableSyncMem(entries: Int=128, ways: Int=2, probabilistic: Boolean = false)(implicit p: Parameters) extends InstructionSliceTable {
  // TODO: Enable bypass from mark->in_ist

  require(ways<=2)
  // First the actual Cache with tag, valids and lru
  val tag_tables = (0 until ways).map(i =>
    Module(new MultiWriteSram(entries/ways, boomParams.ibdaParams.get.ibda_tag_sz, reads=decodeWidth, writes=ibdaParams.rdtIstMarkWidth))
  )

  val tag_valids = (0 until ways).map(_ => RegInit(VecInit(Seq.fill(entries/ways)(false.B)))) //TODO: Use SyncReadMem

  val tag_lru = Module(new MultiWriteSramSimple(entries/2,1, ibdaParams.rdtIstMarkWidth, decodeWidth+ibdaParams.rdtIstMarkWidth, synchronous = false))
  tag_lru.io.write.foreach(w => {
    w.en := false.B
    w.data := DontCare
    w.addr := DontCare
  })
  // Stage 1
  val ist1_check_valid = Wire(Vec(decodeWidth, Bool()))
  val ist1_check_tag = Wire(Vec(decodeWidth, UInt(ibdaParams.ibda_tag_sz.W)))
  val ist1_mark_valid  = Wire(Vec(ibdaParams.rdtIstMarkWidth, Bool()))
  val ist1_mark_tag  = Wire(Vec(ibdaParams.rdtIstMarkWidth, UInt(ibdaParams.ibda_tag_sz.W)))
  dontTouch(ist1_check_tag)

  // Stage 2
  val ist2_check_tag = RegNext(ist1_check_tag)
  val ist2_check_valid = RegNext(ist1_check_valid)
  val ist2_check_sram_tag = Wire(Vec(decodeWidth, Vec(ways, UInt(ibdaParams.ibda_tag_sz.W))))
  val ist2_check_sram_valid = Reg(Vec(decodeWidth, Vec(ways, Bool())))
  val ist2_in_ist = Wire(Vec(decodeWidth, Valid(Bool())))
  dontTouch(ist2_check_sram_tag)

  require(entries == 128)
  require(ways == 2)
  def index(i: UInt): UInt = {
    val indexBits = log2Up(entries/ways)
    val index = Wire(UInt(indexBits.W))
    if (ibdaParams.ibdaTagType == IBDA_TAG_FULL_PC || ibdaParams.ibdaTagType == IBDA_TAG_DEBUG_PC) {
      // xor the second lowest bit with the highest index bit so compressed insns are spread around
      index := i(indexBits+2-1, 2) ^ Cat(i(1), 0.U((indexBits-1).W))
    } else if (ibdaParams.ibdaTagType == IBDA_TAG_INST_LOB) {
      index := Cat(i(12), i(13), i(5,2)) ^ Cat(i(1), 0.U((indexBits-1).W))
      // TODO: Research the entropy in the instruction encoding?
    } else if (ibdaParams.ibdaTagType == IBDA_TAG_UOPC_LOB) {
      index := i(indexBits+2-1,2) ^ Cat(i(1), 0.U((indexBits-1).W))
    }else if (ibdaParams.ibdaTagType == IBDA_TAG_HASH) {
      index := i(indexBits-1,0)
    }
    index
  }

  // Stage 1
  for (i <- 0 until decodeWidth) {
    // Connect to IO
    ist1_check_valid(i) := io.check(i).tag.valid
    ist1_check_tag(i) := io.check(i).tag.bits
  }

  for (i <- 0 until ibdaParams.rdtIstMarkWidth) {
    ist1_mark_valid(i) := io.mark(i).mark.valid
    ist1_mark_tag(i) := io.mark(i).mark.bits
  }

  // Do a cache read
  for(i <- 0 until decodeWidth) {
    val tag = ist1_check_tag(i)
    // needs to be a wire as per https://github.com/freechipsproject/chisel3/issues/1366
    val idx = WireInit(index(tag))
    idx.suggestName(s"sram_read_addr_$i")
    dontTouch(idx)
    for (j <- 0 until ways) {
      // TODO: use read with enable based on valids
      tag_tables(j).io.read(i).addr := idx
      ist2_check_sram_tag(i)(j) := tag_tables(j).io.read(i).data
      if(!probabilistic) {
        ist2_check_sram_valid(i)(j) := tag_valids(j)(idx)
      } else{
        ist2_check_sram_valid(i)(j) := true.B
      }
    }
  }

  // Stage 2
  for (i <- 0 until decodeWidth) {
    ist2_in_ist(i).valid := ist2_check_valid(i)
    ist2_in_ist(i).bits := false.B
    val idx = index(ist2_check_tag(i))
    for(j <- 0 until ways) {
      when(ist2_check_valid(i) && ist2_check_sram_valid(i)(j) && (ist2_check_sram_tag(i)(j) === ist2_check_tag(i))) {
        tag_lru.io.write(i).addr := idx
        tag_lru.io.write(i).en := true.B
        tag_lru.io.write(i).data:= j.U // TODO: fix LRU hack
        ist2_in_ist(i).bits := true.B
      }
    }
    io.check(i).in_ist := ist2_in_ist(i)
  }

  tag_tables.foreach(s => s.io.write.foreach(w => {
    w.en := false.B
    w.data := DontCare
    w.addr := DontCare
  }))
  // mark - later so mark lrus get priority
  for(i <- 0 until ibdaParams.rdtIstMarkWidth){
    tag_lru.io.read(i).addr := DontCare
    when(ist1_mark_valid(i)){
      val idx = index(ist1_mark_tag(i))
      tag_lru.io.read(i).addr := idx
      when(tag_lru.io.read(i).data === 1.U){
        tag_tables(0).io.write(i).addr := idx
        tag_tables(0).io.write(i).data := ist1_mark_tag(i)
        tag_tables(0).io.write(i).en := true.B
        if(!probabilistic) {
          tag_valids(0)(idx) := true.B
        }
        tag_lru.io.write(decodeWidth+i).addr := idx
        tag_lru.io.write(decodeWidth+i).en := true.B
        tag_lru.io.write(decodeWidth+i).data:= 0.U
      }.otherwise{
        tag_tables(1).io.write(i).addr := idx
        tag_tables(1).io.write(i).data := ist1_mark_tag(i)
        tag_tables(1).io.write(i).en := true.B
        if(!probabilistic) {
          tag_valids(1)(idx) := true.B
        }
        tag_lru.io.write(decodeWidth+i).addr := idx
        tag_lru.io.write(decodeWidth+i).en := true.B
        tag_lru.io.write(decodeWidth+i).data:= 1.U
      }
    }
  }
}


class InstructionSliceTableBasic(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends BoomModule{
  val io = IO(new IstIO)
  require(isPow2(entries))
  require(isPow2(ways))

  val tag_table = Reg(Vec(entries, UInt(boomParams.ibdaParams.get.ibda_tag_sz.W)))
  val tag_valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val tag_lru = RegInit(VecInit(Seq.fill(entries/2)(false.B)))

  val ibdaParams = boomParams.ibdaParams.get

  require(entries == 128)
  require(ways == 2)
  def index(i: UInt): UInt = {

    val indexBits = log2Up(entries/ways)
    val index = Wire(UInt(indexBits.W))
    if (ibdaParams.ibdaTagType == IBDA_TAG_FULL_PC) {
      // xor the second lowest bit with the highest index bit so compressed insns are spread around
      index := i(indexBits+2-1, 2) ^ Cat(i(1), 0.U((indexBits-1).W))
    } else if (ibdaParams.ibdaTagType == IBDA_TAG_INST_LOB) {
      index := Cat(i(12), i(13), i(5,2)) ^ Cat(i(1), 0.U((indexBits-1).W))
      // TODO: Research the entropy in the instruction encoding?
    } else if (ibdaParams.ibdaTagType == IBDA_TAG_UOPC_LOB) {
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
  for(i <- 0 until ibdaParams.rdtIstMarkWidth){
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
