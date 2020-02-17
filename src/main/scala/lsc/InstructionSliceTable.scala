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
  val mark = Input(new IstMark)
  val check = Vec(coreWidth, new IstCheck)
}

class IstMark(implicit p: Parameters) extends BoomBundle
{
  val mark = Vec(retireWidth*2, ValidIO(UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W)))
}

class InstructionSliceTable(entries: Int=128, ways: Int=2)(implicit p: Parameters) extends BoomModule{
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
  for(i <- 0 until retireWidth*2){
    when(io.mark.mark(i).valid){
      val pc = io.mark.mark(i).bits
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
