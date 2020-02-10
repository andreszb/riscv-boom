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

class RdtCommitSignals(implicit p: Parameters) extends BoomBundle
{
  val rob = new CommitSignals()
  val tag  = Vec(retireWidth, UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W))
}

class RdtIO(implicit p: Parameters) extends BoomBundle
{
  val commit = Input(new RdtCommitSignals)
  val mark = Output(new IstMark)
}



class RdtIstFIFOSimple(numEntries: Int = 16,
                       numEnqueues: Int = 2,
                       numDequeues: Int = 1)
                      (implicit p: Parameters) extends BoomModule {
  val LscParams = boomParams.loadSliceCore.get
  val dataSz = LscParams.ibda_tag_sz()

  // This type of FIFO only supports Deq-width less or equal to Enq-width
  require(numDequeues <= numEnqueues)

  val io = IO(new Bundle {
    val enq = Vec(numEnqueues, Flipped(DecoupledIO(UInt(dataSz.W))))
    val deq = Vec(numDequeues, DecoupledIO(UInt(dataSz.W)))
  })


  val fifos: Seq[Queue[UInt]] = Seq.fill(numEnqueues)(new Queue(UInt((dataSz+1).W), entries=numEntries, pipe=true, flow=true))


  val any_valid = io.enq.map(_.valid).reduce(_ || _) // Do we have any valid enqueues this CC?
  val any_ready = fifos.head.io.enq.ready //Since we enqueue/dequeue all fifos together, we can use ready signal from one to check if full
  val will_fire = any_valid && any_ready // Will we fire this rounc?
  for(i <- 0 until numEnqueues) {
    io.enq(i).ready := any_ready
    fifos(i).io.enq.bits := Cat(io.enq(i).bits, io.enq(i).valid) // Add the valid bit
    fifos(i).io.enq.valid := any_valid
  }


  var deq_idx = 0
  var all_head_served = true
  for (i <- 0 until numEnqueues) {
    val head = fifos(i).io.deq
    when(head.valid && io.deq(i).ready && head.bits.asUInt()(dataSz,dataSz+1) === 1.U) {
      if (deq_idx < numDequeues) {
        io.deq(deq_idx).valid := true.B
        io.deq(deq_idx).bits := head.bits.asUInt()(0, dataSz)
        head.bits.asUInt()(dataSz, dataSz + 1) := 0.U // Mark as read
      } else {
        all_head_served = false // We werent able to read all the heads. Must wait until next CC
      }
      deq_idx += 1
    }
  }

  // Dequeue all FIFOs in same CC
  fifos.map(_.io.deq.ready := all_head_served.B)

}

class RegisterDependencyTable(implicit p: Parameters) extends BoomModule{
  val io = IO(new RdtIO)

  val rdt = Reg(Vec(32, UInt(boomParams.loadSliceCore.get.ibda_tag_sz.W))) //32 is hardcoded for now since logicalRegCount includes fpu regs
  val commit_dst_valid = WireInit(VecInit(Seq.fill(retireWidth)(false.B)))

  io.mark := DontCare
  io.mark.mark.map(_.valid := false.B)

  for(i <- 0 until retireWidth){
    val uop = io.commit.rob.uops(i)
    // record pc of last insn that committed to reg
    // exclude 0 reg
    when(io.commit.rob.valids(i) && uop.dst_rtype === RT_FIX && uop.ldst =/= 0.U){
      rdt(uop.ldst) := io.commit.tag(i)
      commit_dst_valid(i) := true.B
    }

    val is_b = uop.is_lsc_b || uop.uopc === uopLD || uop.uopc === uopSTA || uop.uopc === uopSTD
    // add pcs of dependent insns to ist
    when(io.commit.rob.valids(i) && is_b){
      when(uop.lrs1_rtype === RT_FIX && uop.lrs1 =/= 0.U){
        io.mark.mark(2*i).valid := true.B
        io.mark.mark(2*i).bits := rdt(uop.lrs1)
        // bypass rdt for previous insns committed in same cycle
        for (j <- 0 until i){
          val uop_j = io.commit.rob.uops(j)
          when(commit_dst_valid(j) && uop_j.ldst === uop.lrs1){
            io.mark.mark(2*i).bits := io.commit.tag(j)
          }
        }
      }
      when(uop.lrs2_rtype === RT_FIX && uop.lrs2 =/= 0.U){
        io.mark.mark(2*i+1).valid := true.B
        io.mark.mark(2*i+1).bits := rdt(uop.lrs2)
        // bypass rdt for previous insns committed in same cycle
        for (j <- 0 until i){
          val uop_j = io.commit.rob.uops(j)
          when(commit_dst_valid(j) && uop_j.ldst === uop.lrs2){
            io.mark.mark(2*i).bits := io.commit.tag(j)
          }
        }
      }
    }
  }
}