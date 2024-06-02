package boom.exu

import scala.math.ceil

import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._

// class ReConFifoItem (implicit p: Parameters) extends BoomBundle {
//   val valid = Bool()
//   val addr = UInt(coreMaxAddrBits.W)
// }

// This module performs two main tasks:
// 1. Track pairs of destination and source register pairs and their address.
// 2. Output the tracked address when a pair is found.
// - When 'en' is HIGH, it stores 'addr' at the 'dst_reg' location in the table.
// - When 'en' is HIGH and the location is not '0.U', it clears the contents and enqueues them for output.
// - 

class LPTIO (implicit p: Parameters) extends BoomBundle {
    // Pair tracking
    val en       = Input (Bool())
    val addr_in  = Input (UInt(coreMaxAddrBits.W))
    val dst_reg  = Input (UInt(numIntPhysRegs.W))
    val src_reg  = Input (UInt(numIntPhysRegs.W))
    // Address output
    val addr_out = new DecoupledIO(UInt(coreMaxAddrBits.W))
}

class LPT (implicit p: Parameters) extends BoomModule {
  val io = IO(new LPTIO)
  val rob_lpt_active = Reg(Vec(numIntPhysRegs, Bool()))
  val rob_lpt_addr   = Reg(Vec(numIntPhysRegs, UInt(coreMaxAddrBits.W)))

  val fifo_in = Wire(Flipped(new DecoupledIO(UInt(coreMaxAddrBits.W))))
  val fifo = Queue(fifo_in, 10)
  io.addr_out <> fifo
  fifo_in.valid := false.B
  fifo_in.bits  := 0.U

  when(io.en){
    rob_lpt_active(io.dst_reg) := true.B
    rob_lpt_addr(io.dst_reg)   := io.addr_in
    when(rob_lpt_active(io.src_reg)===true.B)
    {
      when(fifo_in.ready)
      {
        fifo_in.valid := true.B
        fifo_in.bits  := rob_lpt_addr(io.src_reg)
        rob_lpt_active(io.src_reg) := false.B
        rob_lpt_addr(io.src_reg) := 0.U
      }
    }
  }  

}