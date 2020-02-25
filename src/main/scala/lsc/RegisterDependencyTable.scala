package lsc

import boom.exu.CommitSignals
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._
import freechips.rocketchip.util.PopCountAtLeast

/**
  * Custom commit signals combining ROBs commit signals
  * with a tag that could arrive from somewhere else (like the FTQ)
  * @param
  */

abstract class RegisterDependencyTable(implicit p: Parameters) extends BoomModule {
  val lscParams = boomParams.loadSliceCore.get
  val io = IO(new RdtIO)
}


class RdtIO(implicit p: Parameters) extends BoomBundle
{
  val update = Input(Vec(decodeWidth, new RdtUpdateSignals))
  val mark = Output(Vec(boomParams.loadSliceCore.get.rdtIstMarkWidth, new IstMark))
}

class RdtUpdateSignals(implicit p: Parameters) extends BoomBundle
{
  val valid = Bool()
  val tag  = UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W)
  val uop = new MicroOp
}

class RdtSyncMem(implicit p: Parameters) extends RegisterDependencyTable {

  val rdt = SyncReadMem(boomParams.numIntPhysRegisters, UInt(lscParams.ibda_tag_sz.W))
  val in_ist = RegInit(VecInit(Seq.fill(boomParams.numIntPhysRegisters)(false.B)))

}


class RdtOneBit(implicit p: Parameters) extends RegisterDependencyTable {

  val rdt = SyncReadMem(boomParams.numIntPhysRegisters, UInt(lscParams.ibda_tag_sz.W))
  val in_ist = RegInit(VecInit(Seq.fill(boomParams.numIntPhysRegisters)(false.B)))
  val commit_dst_valid = WireInit(VecInit(Seq.fill(decodeWidth)(false.B)))

  // SyncReadMem is updated next rising edge. If reading location the next CC we have undefined behaviour.
  //  It is probably very common to have such dependency chains and we therefore store the previous update
  //  for bypass


  val update_dst_l = Reg(Vec(decodeWidth, UInt(log2Ceil(boomParams.numIntPhysRegisters).W)))
  val update_tag_l = Reg(Vec(decodeWidth, UInt(lscParams.ibda_tag_sz().W)))
  val update_in_ist_l = Reg(Vec(decodeWidth, Bool()))

  // To track which ports have already been used to mark and when we are full
  val mark_port_idx = Wire(Vec(decodeWidth * 2, UInt(lscParams.rdtIstMarkSz.W)))
  val mark_port_used = WireInit(VecInit(Seq.fill(decodeWidth*2)(false.B)))
  val mark_port_full = Wire(Vec(decodeWidth * 2, Bool()))

  // Output signals
  val mark_valid = WireInit(VecInit(Seq.fill(lscParams.rdtIstMarkWidth)(false.B)))
  val mark_tag_mem_idx = Wire(Vec(lscParams.rdtIstMarkWidth,  UInt(log2Ceil(boomParams.numIntPhysRegisters).W)))
  val mark_tag_bypass_last = WireInit(VecInit(Seq.fill(lscParams.rdtIstMarkWidth)(0.U(lscParams.ibda_tag_sz().W))))
  val mark_tag_bypass_curr = WireInit(VecInit(Seq.fill(lscParams.rdtIstMarkWidth)(0.U(lscParams.ibda_tag_sz().W))))
  val mark_tag_select = WireInit(VecInit(Seq.fill(lscParams.rdtIstMarkWidth)(1.U(3.W))))

  dontTouch(mark_tag_mem_idx)
  dontTouch(mark_tag_bypass_last)
  dontTouch(mark_tag_bypass_curr)
  dontTouch(mark_tag_select)

  mark_tag_mem_idx.map(_ := 0.U) //Default map to address 0


  for (w <- 0 until lscParams.rdtIstMarkWidth) {
    io.mark(w).mark.valid := RegNext(mark_valid(w))
    io.mark(w).mark.bits  := MuxCase(0.U, Array( mark_tag_select(w)(0).asBool() -> rdt(mark_tag_mem_idx(w)),
                                                            mark_tag_select(w)(1).asBool() -> RegNext(mark_tag_bypass_last(w)),
                                                            mark_tag_select(w)(2).asBool() -> RegNext(mark_tag_bypass_curr(w)))
                                                           )
  }

  for (i <- 0 until decodeWidth) {
    val uop = io.update(i).uop
    val valid = io.update(i).valid
    val tag = io.update(i).tag

    val is_b = uop.is_lsc_b || uop.uopc === uopLD || uop.uopc === uopSTA || uop.uopc === uopSTD

    // First the logic for getting which mark_port to use and whether it is already full.
    //  The logic is based on a SW for-loop. mark_port_idx of current iteration depends on the last iteration
    //  mark_port_full of this iteration depends on whether last iteration were already full or if it used the port
    //  and we now are full. It is a little messy since we are looping through 2 ports per iteration (1 per source register)

    if (i == 0) { // Special case of first dispatched instr. At this point we dont depend on any previous iteration
      mark_port_idx(0) := 0.U
      mark_port_full(0) := false.B

    } else { // Normal case
      mark_port_idx(2 * i + 0) := Mux(mark_port_used(2 * i - 1), mark_port_idx(2 * i - 1) + 1.U, mark_port_idx(2 * i - 1))
      mark_port_full(2 * i + 0) := Mux(mark_port_used(2 * i - 1) &&
        mark_port_idx(2 * i) === lscParams.rdtIstMarkWidth.asUInt(), true.B, mark_port_full(2 * i - 1))
    }

    // Can we try to mark RS2?
    mark_port_idx(2 * i + 1) := Mux(mark_port_used(2 * i), mark_port_idx(2 * i) + 1.U, mark_port_idx(2 * i))
    mark_port_full(2 * i + 1) := Mux(mark_port_used(2 * i) &&
      mark_port_idx(2 * i + 1) === lscParams.rdtIstMarkWidth.asUInt(),
      true.B, mark_port_full(2 * i))


    // record pc of last insn that writes to reg
    when(valid && uop.dst_rtype === RT_FIX && uop.pdst =/= 0.U) {
      rdt(uop.pdst) := tag
      in_ist(uop.pdst) := is_b
      commit_dst_valid(i) := true.B
      update_dst_l(i) := uop.pdst
      update_tag_l(i) := tag
      update_in_ist_l(i) := is_b
    }.otherwise {
      update_dst_l(i) := 0.U
    }
    // Mark source register 1
    when(valid && is_b) {
      val port_idx = mark_port_idx(2 * i)
      val port_used = mark_port_used(2 * i)
      val port_full = mark_port_full(2 * i)
      when(uop.lrs1_rtype === RT_FIX && uop.prs1 =/= 0.U) {
        when(!port_full) {
          mark_valid(port_idx) := !in_ist(uop.prs1) // Only valid when RS1 is not already in IST
          port_used := !in_ist(uop.prs1)
          mark_tag_mem_idx(port_idx) := uop.prs1
          mark_tag_select(port_idx) := 1.U

          // Bypass RDT for instructions writing to that location in last cycle
          for (j <- 0 until decodeWidth) {
            when(update_dst_l(j) === uop.prs1) {
              mark_valid(port_idx) := !update_in_ist_l(j) // Only valid when RS1 is not already in IST
              port_used := !update_in_ist_l(j)
              mark_tag_bypass_last(port_idx) := update_tag_l(j)
              mark_tag_select(port_idx) := 2.U
            }
          }

          // bypass rdt for previous insns written in same cycle
          for (j <- 0 until i) {
            val uop_j = io.update(j).uop
            val uop_j_in_ist = (uop_j.is_lsc_b || uop_j.uopc === uopLD || uop_j.uopc === uopSTA || uop_j.uopc === uopSTD)
            when(commit_dst_valid(j) && uop_j.pdst === uop.prs1) {
              mark_valid(port_idx) := !uop_j_in_ist
              port_used := !uop_j_in_ist
              mark_tag_bypass_curr(port_idx) := io.update(j).tag
              mark_tag_select(port_idx) := 4.U
            }
          }
          when(port_used) {
            in_ist(uop.prs1) := true.B
          }
        }
      }


      // Mark source register 2
      when(uop.lrs2_rtype === RT_FIX &&
        uop.prs2 =/= 0.U &&
        !(uop.uopc === uopSTA || uop.uopc === uopSTD)) { //Dont mark RS2 of stores, since its the data generation
        val port_idx = mark_port_idx(2 * i + 1)
        val port_used = mark_port_used(2 * i + 1)
        val port_full = mark_port_full(2 * i + 1)
        when(!port_full) {
          mark_valid(port_idx) := !in_ist(uop.prs2)
          mark_tag_mem_idx(port_idx) := uop.prs2
          port_used := !in_ist(uop.prs2)
          mark_tag_select(port_idx) := 1.U
          // Bypass RDT for instructions writing to that location in last cycle
          for (j <- 0 until decodeWidth) {
            when(update_dst_l(j) === uop.prs2) {
              mark_valid(port_idx) := !update_in_ist_l(j)
              port_used := !update_in_ist_l(j)
              mark_tag_bypass_last(port_idx) := update_tag_l(j)
              mark_tag_select(port_idx) := 2.U
            }
            // bypass rdt for previous insns written in same cycle
            for (j <- 0 until i) {
              val uop_j = io.update(j).uop
              val uop_j_in_ist = (uop_j.is_lsc_b || uop_j.uopc === uopLD || uop_j.uopc === uopSTA || uop_j.uopc === uopSTD)
              when(commit_dst_valid(j) && uop_j.pdst === uop.prs2) {
                mark_valid(port_idx) := !uop_j_in_ist
                port_used := !uop_j_in_ist
                mark_tag_bypass_curr(port_idx) := io.update(j).tag
                mark_tag_select(port_idx) := 4.U
              }
            }
            when(port_used) {
              in_ist(uop.prs2) := true.B
            }
          }
        }
      }
    }
  }

  assert(PopCount(mark_port_used) <= lscParams.rdtIstMarkWidth.U, "[RDT] try to mark more regs than mark-ports")
  assert(PopCount(RegNext(mark_port_used)) === PopCount(io.mark.map(_.mark.valid)), "[RDT] used_ports doesnt match valid")
  assert(PopCount(mark_tag_select(0)) === 1.U, "[RDT] mark_tag_select several hot")
}




class RdtBasic(implicit p: Parameters) extends RegisterDependencyTable {

  val rdt = Reg(Vec(boomParams.numIntPhysRegisters, UInt(lscParams.ibda_tag_sz.W)))
  val commit_dst_valid = WireInit(VecInit(Seq.fill(decodeWidth)(false.B)))

  io.mark := DontCare
  io.mark.map(_.mark.valid := false.B)

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
        io.mark(2*i).mark.valid := true.B
        io.mark(2*i).mark.bits := rdt(uop.prs1)
        // bypass rdt for previous insns committed in same cycle
        for (j <- 0 until i){
          val uop_j = io.update(j).uop
          when(commit_dst_valid(j) && uop_j.pdst === uop.prs1){
            io.mark(2*i).mark.bits := io.update(j).tag
          }
        }
      }
      when(uop.lrs2_rtype === RT_FIX && uop.prs2 =/= 0.U){
        io.mark(2*i+1).mark.valid := true.B
        io.mark(2*i+1).mark.bits := rdt(uop.prs2)
        // bypass rdt for previous insns committed in same cycle
        for (j <- 0 until i){
          val uop_j = io.update(j).uop
          when(commit_dst_valid(j) && uop_j.pdst === uop.prs2){
            io.mark(2*i+1).mark.bits := io.update(j).tag
          }
        }
      }
    }
  }
}