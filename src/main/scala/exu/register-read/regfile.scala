//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Register File (Abstract class and Synthesizable RegFile)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import scala.collection.mutable.ArrayBuffer
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.util.BoomCoreStringPrefix
import lsc.MultiWriteSram

/**
 * IO bundle for a register read port
 *
 * @param addrWidth size of register address in bits
 * @param dataWidth size of register in bits
 */
class RegisterFileReadPortIO(val addrWidth: Int, val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
{
  val addr = Input(UInt(addrWidth.W))
  val data = Output(UInt(dataWidth.W))
}

/**
 * IO bundle for the register write port
 *
 * @param addrWidth size of register address in bits
 * @param dataWidth size of register in bits
 */
class RegisterFileWritePort(val addrWidth: Int, val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
{
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
}

/**
 * Utility function to turn ExeUnitResps to match the regfile's WritePort I/Os.
 */
object WritePort
{
  def apply(enq: DecoupledIO[ExeUnitResp], addrWidth: Int, dataWidth: Int, rtype: UInt)
    (implicit p: Parameters): Valid[RegisterFileWritePort] = {
     val wport = Wire(Valid(new RegisterFileWritePort(addrWidth, dataWidth)))

     wport.valid     := enq.valid && enq.bits.uop.dst_rtype === rtype
     wport.bits.addr := enq.bits.uop.pdst
     wport.bits.data := enq.bits.data
     enq.ready       := true.B
     wport
  }
}

/**
 * Register file abstract class
 *
 * @param numRegisters number of registers
 * @param numReadPorts number of read ports
 * @param numWritePorts number of write ports
 * @param registerWidth size of registers in bits
 * @param bypassableArray list of write ports from func units to the read port of the regfile
 */
abstract class RegisterFile(
  numRegisters: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  registerWidth: Int,
  bypassableArray: Seq[Boolean]) // which write ports can be bypassed to the read ports?
  (implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val read_ports = Vec(numReadPorts, new RegisterFileReadPortIO(maxPregSz, registerWidth))
    val write_ports = Flipped(Vec(numWritePorts, Valid(new RegisterFileWritePort(maxPregSz, registerWidth))))
  })

  private val rf_cost = (numReadPorts + numWritePorts) * (numReadPorts + 2*numWritePorts)
  private val type_str = if (registerWidth == fLen+1) "Floating Point" else "Integer"
  override def toString: String = BoomCoreStringPrefix(
    "==" + type_str + " Regfile==",
    "Num RF Read Ports     : " + numReadPorts,
    "Num RF Write Ports    : " + numWritePorts,
    "RF Cost (R+W)*(R+2W)  : " + rf_cost,
    "Bypassable Units      : " + bypassableArray)
}

/**
 * A synthesizable model of a Register File. You will likely want to blackbox this for more than modest port counts.
 *
 * @param numRegisters number of registers
 * @param numReadPorts number of read ports
 * @param numWritePorts number of write ports
 * @param registerWidth size of registers in bits
 * @param bypassableArray list of write ports from func units to the read port of the regfile
 */
class RegisterFileSynthesizable(
   numRegisters: Int,
   numReadPorts: Int,
   numWritePorts: Int,
   registerWidth: Int,
   bypassableArray: Seq[Boolean])
   (implicit p: Parameters)
   extends RegisterFile(numRegisters, numReadPorts, numWritePorts, registerWidth, bypassableArray)
{
  // --------------------------------------------------------------

//  val regfile = Mem(numRegisters, UInt(registerWidth.W))
  val regfile = Module(new MultiWriteSram(size=numRegisters, width = registerWidth, reads=numReadPorts, writes = numWritePorts))

  // --------------------------------------------------------------
  // Read ports.

  val read_data = Wire(Vec(numReadPorts, UInt(registerWidth.W)))

  // Register the read port addresses to give a full cycle to the RegisterRead Stage (if desired).
  val read_addrs = io.read_ports.map(p => RegNext(p.addr))

  for (i <- 0 until numReadPorts) {
    regfile.io.read(i).addr := io.read_ports(i).addr
    read_data(i) := regfile.io.read(i).data//regfile(read_addrs(i))
  }

  // --------------------------------------------------------------
  // Bypass out of the ALU's write ports.
  // We are assuming we cannot bypass a writer to a reader within the regfile memory
  // for a write that occurs at the end of cycle S1 and a read that returns data on cycle S1.
  // But since these bypasses are expensive, and not all write ports need to bypass their data,
  // only perform the w->r bypass on a select number of write ports.

  require (bypassableArray.length == io.write_ports.length)

//  if (bypassableArray.reduce(_||_)) {
    val bypassable_wports = ArrayBuffer[Valid[RegisterFileWritePort]]()
    io.write_ports zip bypassableArray map { case (wport, b) => if (b) { bypassable_wports += wport} }
    // as the sequential memmory might not allow reads during writes we need this
    bypassable_wports ++=  io.write_ports.map(RegNext(_))

    for (i <- 0 until numReadPorts) {
      val bypass_ens = bypassable_wports.map(x => x.valid &&
        x.bits.addr === read_addrs(i))
//      assert(PopCount(bypass_ens) <= 1.U, "multiple bypasses at once")
      // use a priority mus because the asserion above is not true
      val bypass_data = PriorityMux(VecInit(bypass_ens), VecInit(bypassable_wports.map(_.bits.data)))

      io.read_ports(i).data := Mux(bypass_ens.reduce(_|_), bypass_data, read_data(i))
    }
//  } else {
//    require(false)
//    for (i <- 0 until numReadPorts) {
//      io.read_ports(i).data := read_data(i)
//    }
//  }

  // --------------------------------------------------------------
  // Write ports.

//  for (wport <- io.write_ports) {
//    when (wport.valid) {
//      regfile(wport.bits.addr) := wport.bits.data
//    }
//  }
  for(i <- 0 until numWritePorts){
    regfile.io.write(i).en := io.write_ports(i).valid
    regfile.io.write(i).addr := io.write_ports(i).bits.addr
    regfile.io.write(i).data := io.write_ports(i).bits.data
  }

  // ensure there is only 1 writer per register (unless to preg0)
  if (numWritePorts > 1) {
    for (i <- 0 until (numWritePorts - 1)) {
      for (j <- (i + 1) until numWritePorts) {
        assert(!io.write_ports(i).valid ||
               !io.write_ports(j).valid ||
               (io.write_ports(i).bits.addr =/= io.write_ports(j).bits.addr) ||
               (io.write_ports(i).bits.addr === 0.U), // note: you only have to check one here
          "[regfile] too many writers a register")
      }
    }
  }
}
