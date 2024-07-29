package boom.lsu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket._

import boom.common._

class reConTableIO (implicit p: Parameters) extends BoomBundle with HasL1HellaCacheParameters {
  val en   = Input(Bool())
  val check = Input(Bool())
  val clr  = Input(Bool())
  val way  = Input(UInt(nWays.W))
  val addr = Input(UInt(coreMaxAddrBits.W))
  val out  = Output(Bool())
}

class reConTable (implicit p: Parameters) extends BoomModule with HasL1HellaCacheParameters
{
    
    val io    = IO(new reConTableIO)
    var reCon = Reg(Vec(nWays, Vec(nSets * refillCycles, Vec(rowWords, Bool()))))
    val way   = PriorityEncoder(io.way)
    val set   = io.addr(untagBits-1,rowOffBits)
    val word  = if (rowWords == 1) 0.U else io.addr(log2Up(rowWords*wordBytes)-1, log2Up(wordBytes))
    when(io.clr || io.en){
      reCon(way)(set)(word) := !io.clr && io.en
      when (reCon(way)(set)(word) =/= (!io.clr && io.en)){
        // when(io.clr){
        //   printf("set_conceal\n")
        // }.elsewhen(io.en){
        //   printf("set_reveal\n")
        // }
      }
    }
    io.out  := reCon(way)(set)(word) && io.check
    // when (io.check) {
    //   when (reCon(way)(set)(word)) {
    //     printf("get_reveal\n")
    //   } .otherwise {
    //     printf("get_conceal\n")
    //   }
    // }
}