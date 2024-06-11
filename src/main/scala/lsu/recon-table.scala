package boom.lsu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._

import boom.common._

class reConTableIO (implicit p: Parameters) extends BoomBundle {
  val en   = Input(Bool())
  val check = Input(Bool())
  val clr  = Input(Bool())
  val way  = Input(UInt())
  val addr = Input(UInt(coreMaxAddrBits.W))
  val out  = Output(Valid(Bool()))
}

class reConTable (implicit p: Parameters) extends BoomModule with HasL1HellaCacheParameters
{
    
    val io = IO(new reConTableIO)
    dontTouch(io.en)
    dontTouch(io.check)
    dontTouch(io.clr)
    dontTouch(io.way)
    dontTouch(io.addr)
    dontTouch(io.out)
    var reCon = Reg(Vec(nWays, Vec(nSets, Vec(8, Bool()))))
    // var reCon = Reg(Vec(nWays, Vec(nSets, Vec(rowWords, Bool()))))
    dontTouch(reCon)
    val set  = io.addr(11, 6)
    dontTouch(set)
    val word = io.addr(5, 3)
    dontTouch(word)

    when(io.clr || io.en){
      reCon(io.way-1.U)(set)(word) := !io.clr && io.en
    }

    io.out.valid := io.check
    io.out.bits  := reCon(io.way-1.U)(set)(word)
}