package boom.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str
import boom.common._

// This module performs two main tasks:
// 1. Track pairs of destination and source register pairs and their address.
// 2. Output the tracked address when a pair is found.
class LPTIO (implicit p: Parameters) extends BoomBundle {
  val en  = Input(Bool())
  val clr = Input(Bool())
  val addr_in       = Input(UInt(coreMaxAddrBits.W))
  val dst_reg       = Input(UInt(numIntPhysRegs.W))
  val src_reg       = Input(UInt(numIntPhysRegs.W))
  val addr_out      = new DecoupledIO(UInt(coreMaxAddrBits.W))
}

class LPT (implicit p: Parameters) extends BoomModule {

  val io = IO(new LPTIO)
  val fifo_in = Wire(Flipped(new DecoupledIO(UInt(coreMaxAddrBits.W))))
  val fifo = Queue(fifo_in, 10)
  val table = Reg(Vec(numIntPhysRegs, Valid(UInt(coreMaxAddrBits.W))))

  io.addr_out <> fifo
  fifo_in.valid := false.B
  fifo_in.bits  := 0.U

  when(io.clr){
    table(io.dst_reg).valid := false.B
    table(io.dst_reg).bits  := 0.U
  }.elsewhen (io.en) {
    table(io.dst_reg).valid := true.B
    table(io.dst_reg).bits := io.addr_in
    when(table(io.src_reg).valid) {
      when(fifo_in.ready) {
        fifo_in.valid := true.B
        fifo_in.bits := table(io.src_reg).bits
        table(io.src_reg).valid := false.B
        table(io.src_reg).bits := 0.U
      }
    }
  }

}
