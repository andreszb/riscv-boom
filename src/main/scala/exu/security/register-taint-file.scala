package boom.exu

import java.nio.file.Paths
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.{Causes, PRV, TraceDoctor}
import freechips.rocketchip.util.{CoreMonitorBundle, SeqToAugmentedSeq, Str, UIntIsOneOf}
import freechips.rocketchip.devices.tilelink.{CLINTConsts, PLICConsts}
import testchipip.ExtendedTracedInstruction
import boom.common._
import boom.ifu.{GlobalHistory}
import boom.exu.FUConstants._
import boom.util._

class RegisterTaintTracker(
  val issueWidth: Int,
  val numPhysRegs: Int,
  val is_fp: Boolean)(implicit p: Parameters) extends BoomModule
    with HasBoomCoreParameters
{

    val io = IO(new Bundle {
        val taint_wakeup_port = Flipped(Vec(numTaintWakeupPorts, Valid(UInt(ldqAddrSz.W))))

        val ldq_flipped    = Input(Bool())

        val req_valids     = Input(Vec(issueWidth, Bool()))
        val req_uops       = Input(Vec(issueWidth, new MicroOp()))

        val req_yrot       = Output(Vec(issueWidth, (UInt(ldqAddrSz.W))))
        val req_yrot_r     = Output(Vec(issueWidth, Bool()))

        val ext_entries_in = Flipped(Vec(issueWidth, Valid(new TaintEntry())))
        val ext_entries_in_idx = Input(Vec(issueWidth, UInt(ldqAddrSz.W)))

        val ext_entries_out  = Output(Vec(issueWidth, Valid(new TaintEntry())))
        val ext_entries_out_idx  = Output(Vec(issueWidth, Valid(UInt(ldqAddrSz.W))))

    })

    val int_type = RT_FIX
    val fp_type = RT_FLT
    val dud_entry = Wire(new TaintEntry())
    val t_rtype = if(is_fp) fp_type else int_type
    dud_entry.valid := false.B
    dud_entry.ldq_idx := 0.U
    dud_entry.flipped_age := false.B

    // Helper function for finding youngest root of taint amongst several TaintEntry options
    def GetTaintEntry(
        uop: MicroOp,
        t1: TaintEntry,
        t2: TaintEntry,
        t3: TaintEntry,
        idx: UInt)
        : TaintEntry = {

        val rs3_en = uop.frs3_en

        val t1_valid = t1.valid
        val t2_valid = t2.valid
        val t3_valid = t3.valid && rs3_en

        val t1_age = t1.ldq_idx 
        val t1_flipped = t1.flipped_age
        val t2_age = t2.ldq_idx 
        val t2_flipped = t2.flipped_age
        val t3_age = t3.ldq_idx 
        val t3_flipped = t3.flipped_age

        val any_valid = t1_valid || t2_valid || t3_valid

        val t1_youngness = Mux(io.ldq_flipped === t1_flipped, 
                                                    numLdqEntries.U - t1_age, (numLdqEntries*2).U - t1_age)
        val t2_youngness = Mux(io.ldq_flipped === t2_flipped,
                                                    numLdqEntries.U - t2_age, (numLdqEntries*2).U - t2_age)
        val t3_youngness = Mux(io.ldq_flipped === t3_flipped,
                                                    numLdqEntries.U - t3_age, (numLdqEntries*2).U - t3_age)
        
        val t1_oldest = (t1_valid 
                        && ((!t2_valid) || t1_youngness <= t2_youngness)
                        && ((!t3_valid) || t1_youngness <= t3_youngness))
        val t2_oldest = ((!t1_oldest)
                        && t2_valid
                        && ((!t3_valid) || t2_youngness <= t3_youngness))
        val t3_oldest = ((!t1_oldest)
                        && (!t2_oldest)
                        && t3_valid) 
        
        assert(!(t1_oldest && t2_oldest))
        assert(!(t1_oldest && t3_oldest))
        assert(!(t2_oldest && t3_oldest)) 

        val chosen_t = Wire(new TaintEntry())

        chosen_t.valid := false.B
        chosen_t.ldq_idx := 0.U
        chosen_t.flipped_age := false.B
        
        when(t1_oldest) {
            chosen_t := t1
        } .elsewhen(t2_oldest) {
            chosen_t := t2
        } .elsewhen(t3_oldest) {
            chosen_t := t3
        }
        
        chosen_t
    }

    // Get the matching TaintEntries from TaintFile for a given UOP
    def GetRegTaints(uop: MicroOp) : (TaintEntry, TaintEntry, TaintEntry) = {

        val t1 = Mux(uop.lrs1_rtype =/= t_rtype, dud_entry, taint_file(uop.lrs1))

        val t2 = Mux(uop.lrs2_rtype =/= t_rtype, dud_entry, taint_file(uop.lrs2))

        val t3 = Mux(uop.frs3_en && uop.lrs1_rtype === t_rtype, taint_file(uop.lrs3), dud_entry)
        
        (t1, t2, t3)
    }

    // Taint files for int and fp registers
    val int_taint_file              = Reg(Vec(numPhysRegs, new TaintEntry()))

    val req_uops                    = Wire(Vec(issueWidth, new MicroOp()))
    val req_valids                  = Wire(Vec(issueWidth, Bool()))

    
    val ext_entries_out         = Reg(Vec(issueWidth, Valid(new TaintEntry())))
    val ext_entries_out_idx     = Reg(Vec(issueWidth, Valid(UInt(ldqAddrSz.W))))

    req_uops := io.req_uops
    req_valids := io.req_valids

    val taint_file_freed_taints = Wire(Vec(numPhysRegs, new TaintEntry()))

    for (i <- 0 until numPhysRegs) {
        taint_file_freed_taints(i) := (io.ext_entries_in zip io.ext_entries_in_idx)
            .foldLeft(taint_file(i))
                {case (entry, (ext_in, ext_idx)) => Mux(ext_in.valid && ext_idx === i.U, ext_in.bits, entry)}
        
        taint_file_freed_taints(i).valid := io.taint_wakeup_port
            .foldLeft(taint_file_freed_taints(i).valid)
                {case (valid, twake) => valid && !(twake.valid && (twake.bits === taint_file_freed_taints(i).ldq_idx))}
    }

    val new_taint_entries = Wire(Vec(issueWidth, new TaintEntry()))

    for (i <- 0 until issueWidth) {
        val t_ent = Wire(new TaintEntry())
        val (t1, t2, t3) = GetRegTaints(req_uops(i))

        t_ent := GetTaintEntry(req_uops(i), t1, t2, t3, i.U)

        val new_tent = WireInit(t_ent)

        when (req_valids(i) && req_uops(i).uses_ldq && (!req_uops(i).is_problematic)) {
            new_tent.ldq_idx := req_uops(i).ldq_idx
            new_tent.flipped_age := req_uops(i).ldq_flipped
            new_tent.valid := true.B
        }.elsewhen(req_valids(i) && (!req_uops(i).is_problematic)) {
            new_taint_entries(i) := t_ent
        }. otherwise {
            new_tent.valid := false.B
        }

        new_taint_entries(i) := new_tent
        io.req_yrot(i) := t_ent.ldq_idx
        io.req_yrot_r(i) := !t_ent.valid
    }

    var ext_entry_count = 0
    for (i <- 0 until numPhysRegs) {
        val remapped_entry = (req_uops.map(uop => (uop.ldst_val, uop.ldst, uop.dst_rtype)) zip req_valids zip new_taint_entries)
            .foldLeft(taint_file_freed_taints(i)) {case (t_ent, (((ldst_val, ldst, rtype), fire), new_t_ent)) =>
                Mux(fire && ldst_val && ldst === i.U && rtype === t_rtype, new_t_ent, t_ent)}

        taint_file(i) := remapped_entry

        val ext_entry = (req_uops.map(uop => (uop.ldst_val, uop.ldst, uop.dst_rtype)) zip req_valids zip new_taint_entries)
            .foldLeft(dud_entry) {case (t_ent, (((ldst_val, ldst, rtype), fire), new_t_ent)) =>
                Mux(fire && ldst_val && ldst === i.U && rtype =/= t_rtype, new_t_ent, t_ent)}
        
        when (ext_entry.valid) {
            ext_entries_out(ext_entry_count).valid := true.B
            ext_entries_out(ext_entry_count).bits := ext_entry
            ext_entries_out_idx(ext_entry_count).bits := i.U
            ext_entry_count += 1
        }
    }

    io.ext_entries_out := ext_entries_out
    io.ext_entries_out_idx := ext_entries_out_idx
/* 
    when (io.rollback || io.exception) {
        for (i <- 0 until numPhysRegs) {
            taint_file(i) := dud_entry
        }
    } */

    dontTouch(taint_file)

}