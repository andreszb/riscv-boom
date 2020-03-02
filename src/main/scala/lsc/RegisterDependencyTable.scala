package lsc

import boom.exu.CommitSignals
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._

/**
  * Custom commit signals combining ROBs commit signals
  * with a tag that could arrive from somewhere else (like the FTQ)
  * @param
  */

class RdtUpdateSignals(implicit p: Parameters) extends BoomBundle
{
  val valid = Bool()
  val tag  = UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W)
  val uop = new MicroOp
}

class RdtPerfCounters(implicit p: Parameters) extends BoomBundle {
  val mark_used = Vec(4, Bool())
}

class RdtIO(implicit p: Parameters) extends BoomBundle
{
  val update = Input(Vec(decodeWidth, new RdtUpdateSignals))
  val mark = Output(new IstMark)
  val perf_counters = Output(new RdtPerfCounters())
}

class RegisterDependencyTable(implicit p: Parameters) extends BoomModule{
  val io = IO(new RdtIO)

  val LscParams = boomParams.loadSliceCore.get

  val rdt = Reg(Vec(boomParams.numIntPhysRegisters, UInt(LscParams.ibda_tag_sz.W)))
  val commit_dst_valid = WireInit(VecInit(Seq.fill(decodeWidth)(false.B)))

  io.mark := DontCare
  io.mark.mark.map(_.valid := false.B)

  for(i <- 0 until decodeWidth){


    val uop = io.update(i).uop
    val valid = io.update(i).valid
    val tag = io.update(i).tag

    // record pc of last insn that committed to reg
    when(valid && uop.dst_rtype === RT_FIX && uop.pdst =/= 0.U){
      rdt(uop.pdst) := tag
      commit_dst_valid(i) := true.B
    }

    val is_b = uop.is_lsc_b || uop.uopc === uopLD || uop.uopc === uopSTA || uop.uopc === uopSTD
    // add pcs of dependent insns to ist
    when(valid && is_b){
      when(uop.lrs1_rtype === RT_FIX && uop.prs1 =/= 0.U){ //TODO: Remove use of logical register specifiers.
        io.mark.mark(2*i).valid := true.B
        io.mark.mark(2*i).bits := rdt(uop.prs1)
        // bypass rdt for previous insns committed in same cycle
        for (j <- 0 until i){
          val uop_j = io.update(j).uop
          when(commit_dst_valid(j) && uop_j.pdst === uop.prs1){
            io.mark.mark(2*i).bits := io.update(j).tag
          }
        }
      }
      when(uop.lrs2_rtype === RT_FIX && uop.prs2 =/= 0.U){
        io.mark.mark(2*i+1).valid := true.B
        io.mark.mark(2*i+1).bits := rdt(uop.prs2)
        // bypass rdt for previous insns committed in same cycle
        for (j <- 0 until i){
          val uop_j = io.update(j).uop
          when(commit_dst_valid(j) && uop_j.pdst === uop.prs2){
            io.mark.mark(2*i+1).bits := io.update(j).tag
          }
        }
      }
    }
  }

  val cnt = PopCount(io.mark.mark.map(_.valid))
  when (cnt === 0.U) {
    io.perf_counters.mark_used.map(_ := false.B)
  }. elsewhen(cnt === 1.U) {
    io.perf_counters.mark_used.map(_ := false.B)
    io.perf_counters.mark_used(0) := true.B
  }. elsewhen(cnt === 2.U) {
    io.perf_counters.mark_used.map(_ := false.B)
    io.perf_counters.mark_used(1) := true.B
  }. elsewhen(cnt === 3.U) {
    io.perf_counters.mark_used.map(_ := false.B)
    io.perf_counters.mark_used(2) := true.B
  }. elsewhen(cnt === 4.U) {
    io.perf_counters.mark_used.map(_ := false.B)
    io.perf_counters.mark_used(3) := true.B
  }


}