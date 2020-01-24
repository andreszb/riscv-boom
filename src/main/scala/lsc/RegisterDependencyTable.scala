package lsc

import boom.exu.CommitSignals
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._

class RdtIO(implicit p: Parameters) extends BoomBundle
{
  val commit = Input(new CommitSignals)
  val mark = Output(new IstMark)
}

class RegisterDependencyTable()(implicit p: Parameters) extends BoomModule{
  val io = IO(new RdtIO())

  val rdt = Reg(Vec(32, UInt((32+5).W))) //32 is hardcoded for now since logicalRegCount includes fpu regs
  val commit_dst_valid = WireInit(VecInit(Seq.fill(retireWidth)(false.B)))

  io.mark := DontCare
  io.mark.mark.map(_.valid := false.B)

  for(i <- 0 until retireWidth){
    val uop = io.commit.uops(i)
    // record pc of last insn that committed to reg
    // exclude 0 reg
    when(io.commit.valids(i) && uop.dst_rtype === RT_FIX && uop.ldst =/= 0.U){
      // TODO: figure out if there is a better pc - guess we shouldn't use debug...
      rdt(uop.ldst) := Cat(uop.inst, uop.pc_lob)
      commit_dst_valid(i) := true.B
    }

    val is_b = uop.is_lsc_b || uop.uopc === uopLD || uop.uopc === uopSTA || uop.uopc === uopSTD
    // add pcs of dependent insns to ist
    when(io.commit.valids(i) && is_b){
      when(uop.lrs1_rtype === RT_FIX && uop.lrs1 =/= 0.U){
        io.mark.mark(2*i).valid := true.B
        io.mark.mark(2*i).bits := rdt(uop.lrs1)
        // bypass rdt for previous insns committed in same cycle
        for (j <- 0 until i){
          val uop_j = io.commit.uops(j)
          when(commit_dst_valid(j) && uop_j.ldst === uop.lrs1){
            io.mark.mark(2*i).bits := Cat(uop_j.inst, uop_j.pc_lob)
          }
        }
      }
      when(uop.lrs2_rtype === RT_FIX && uop.lrs2 =/= 0.U){
        io.mark.mark(2*i+1).valid := true.B
        io.mark.mark(2*i+1).bits := rdt(uop.lrs2)
        // bypass rdt for previous insns committed in same cycle
        for (j <- 0 until i){
          val uop_j = io.commit.uops(j)
          when(commit_dst_valid(j) && uop_j.ldst === uop.lrs2){
            io.mark.mark(2*i).bits := Cat(uop_j.inst, uop_j.pc_lob)
          }
        }
      }
    }
  }
}