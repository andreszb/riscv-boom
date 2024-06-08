package boom.lsu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._

class reConTableIO (implicit p: Parameters) extends BoomBundle {
  val en   = Input(Bool())
  val check = Input(Bool())
  val clr  = Input(Bool())
  val way  = Input(UInt())
  val addr = Input(UInt(coreMaxAddrBits.W))
  val out  = Output(Valid(Bool()))
}

class reConTable (implicit p: Parameters) extends BoomModule
{
    
    val io = IO(new reConTableIO)
    dontTouch(io.en)
    dontTouch(io.check)
    dontTouch(io.clr)
    dontTouch(io.way)
    dontTouch(io.addr)
    dontTouch(io.out)
    var reCon = Reg(Vec(4, Vec(64, Vec(1, Bool()))))
    dontTouch(reCon)
    val set  = io.addr(11, 6)
    dontTouch(set)
    val word = io.addr(5, 3)
    dontTouch(word)

    reCon(io.way)(set)(word) := io.en & ~io.clr
    io.out.valid := io.check
    io.out.bits  := reCon(io.way)(set)(word)
}