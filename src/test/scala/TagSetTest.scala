//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

import chisel3._
import chisel3.iotesters._
import chisel3.util.{ValidIO, log2Up}

class TagSet(entries: Int=128, ways: Int=2, width: Int=40, writeWidth:Int = 1, readWidth:Int = 2, memoryType:String = "sync_mem") extends Module {
  val io = IO(new Bundle{
    val s0_add = Input(Vec(writeWidth, ValidIO(UInt(width.W))))
    val s0_check  = Input(Vec(readWidth, ValidIO(UInt(width.W))))
    val s1_in_set  = Output(Vec(readWidth, ValidIO(Bool())))
    val flush = Input(Bool())
  })
  require(ways<=2)
  def index(i: UInt): UInt = {
    val indexBits = log2Up(entries/ways)
    i(indexBits-1, 0)
  }
  val tag_tables = (0 until ways).map(i => memoryType match {
//    case "reg" => Reg(Vec(entries/ways, UInt(width.W)))
    case "mem_delay_addr" => Mem(entries/ways, UInt(width.W))
    case "mem_delay_data" => Mem(entries/ways, UInt(width.W))
    case "sync_mem" => SyncReadMem(entries/ways, UInt(width.W))
  }
  )
  val tag_valids = (0 until ways).map(_ => RegInit(VecInit(Seq.fill(entries/ways)(false.B))))
  val tag_lru = RegInit(VecInit(Seq.fill(entries/2)(false.B)))

  val s1_check = RegNext(io.s0_check)
  val s1_tag_valids = Reg(Vec(readWidth, Vec(ways, Bool())))
  val s1_sram_tags = Wire(Vec(readWidth, Vec(ways, UInt(width.W))))

  // cycle 0
  for(i <- 0 until readWidth) {
    val idx = index(io.s0_check(i).bits)
    idx.suggestName(s"sram_read_addr_$i")
    dontTouch(idx)
    for (j <- 0 until ways) {
      memoryType match {
        case "mem_delay_addr" => {
          s1_sram_tags(i)(j) := tag_tables(j)(RegNext(idx))
        }
        case "mem_delay_data" => {
          s1_sram_tags(i)(j) := RegNext(tag_tables(j)(idx))
        }
        case "sync_mem" => {
          s1_sram_tags(i)(j) := tag_tables(j)(idx)
        }
      }
      s1_tag_valids(i)(j) := tag_valids(j)(idx)
    }
  }

  // cycle 1
  for (i <- 0 until readWidth) {
    io.s1_in_set(i).valid := s1_check(i).valid
    io.s1_in_set(i).bits := false.B
    val idx = index(s1_check(i).bits)
    for(j <- 0 until ways) {
      when(s1_check(i).valid && s1_tag_valids(i)(j) && (s1_sram_tags(i)(j) === s1_check(i).bits)) {
        tag_lru(idx) := j.B
        io.s1_in_set(i).bits := true.B
      }
    }
  }

  // cycle 0 - mark - later so mark lrus get priority
  for(i <- 0 until writeWidth){
    when(io.s0_add(i).valid){
      val idx = index(io.s0_add(i).bits)
      if(ways == 2) {
        when(tag_lru(idx)) {
          tag_tables(0)(idx) := io.s0_add(i).bits
          tag_valids(0)(idx) := true.B
          tag_lru(idx) := false.B
        }.otherwise {
          tag_tables(1)(idx) := io.s0_add(i).bits
          tag_valids(1)(idx) := true.B
          tag_lru(idx) := true.B
        }
      } else if(ways == 1){
        tag_tables(0)(idx) := io.s0_add(i).bits
        tag_valids(0)(idx) := true.B
      }
    }
  }

  when(io.flush){
    tag_valids.foreach(_.foreach(_ := false.B))
  }
}
/**
 * Main register file tester
 */
class TagSetTester extends ChiselFlatSpec
{
  val entries = 128
  for(memType <- "mem_delay_addr" :: "mem_delay_data" :: "sync_mem" :: Nil) {
    it should s"work for $memType" in
      {
        chisel3.iotesters.Driver(() => new TagSet(memoryType = memType, entries=entries), "verilator")
        {
          (c) => new TagSetTest(c, entries)
        } should be (true)
      }
  }
}

/**
 * Read/writes from register preg0 and make sure that it gets 0
 */
class TagSetTest[R <: TagSet](
  c: R, entries: Int) extends PeekPokeTester(c)
{
  // init everything to false
  poke(c.io.s0_add(0).valid, false.B)
  poke(c.io.s0_add(0).bits, 0.U)
  poke(c.io.s0_check(0).bits, 0.U)
  poke(c.io.s0_check(1).bits, 0.U)
  poke(c.io.s0_check(0).valid, false.B)
  poke(c.io.s0_check(1).valid, false.B)
  poke(c.io.flush, false.B)
  step(1)
  poke(c.io.s0_add(0).valid, true.B)
  // add 0-entries
  for(i <- 0 until entries){
    // check that in_set is only valid when read is ongoing
    expect(c.io.s1_in_set(0).valid, false.B)
    expect(c.io.s1_in_set(1).valid, false.B)
    poke(c.io.s0_add(0).bits, i.U)
    step(1)
  }
  poke(c.io.s0_add(0).valid, false.B)
  step(5)
  // check that 0-entries are in set (i and i+entries/2)
  poke(c.io.s0_check(0).valid, true.B)
  poke(c.io.s0_check(1).valid, true.B)
  for(i <- 0 until entries/2){
    poke(c.io.s0_check(0).bits, i.U)
    poke(c.io.s0_check(1).bits, (i+entries/2).U)
    step(1)
    expect(c.io.s1_in_set(0).valid, true.B)
    expect(c.io.s1_in_set(1).valid, true.B)
    expect(c.io.s1_in_set(0).bits, true.B)
    expect(c.io.s1_in_set(1).bits, true.B)
  }
  // check that some other numbers are not in set
  for(i <- entries until 2*entries){
    poke(c.io.s0_check(0).bits, i.U)
    poke(c.io.s0_check(1).bits, (i+entries).U)
    step(1)
    expect(c.io.s1_in_set(0).valid, true.B)
    expect(c.io.s1_in_set(1).valid, true.B)
    expect(c.io.s1_in_set(0).bits, false.B)
    expect(c.io.s1_in_set(1).bits, false.B)
  }
  step(5)
  // flush
  poke(c.io.flush, true.B)
  step(1)
  poke(c.io.flush, false.B)
  step(1)
  // confirm that flush worked
  for(i <- 0 until entries/2){
    poke(c.io.s0_check(0).bits, i.U)
    poke(c.io.s0_check(1).bits, (i+entries/2).U)
    step(1)
    expect(c.io.s1_in_set(0).valid, true.B)
    expect(c.io.s1_in_set(1).valid, true.B)
    expect(c.io.s1_in_set(0).bits, false.B)
    expect(c.io.s1_in_set(1).bits, false.B)
  }
}