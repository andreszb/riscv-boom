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


class TaintEntry()(implicit p: Parameters) extends BoomBundle
    with HasBoomCoreParameters
{
    val valid           = Bool()
    val ldq_idx         = UInt(ldqAddrSz.W)
    val age             = UInt(ldqAddrSz.W)
    val flipped_age     = Bool()
}

class TaintTracker(
  val plWidth: Int,
  val numLregs: Int)(implicit p: Parameters) extends BoomModule
    with HasBoomCoreParameters
{

    val io = IO(new Bundle {
        val ren1_uops = Input(Vec(plWidth, new MicroOp()))
        val ren1_fire = Input(Vec(plWidth, Bool()))

        val ldq_flipped = Input(Bool())

        val uops_yrot = Output(Vec(plWidth, UInt(ldqAddrSz.W)))

        val ren_br_tags = Input(Vec(plWidth, Valid(UInt(brTagSz.W))))

        val brupdate = Input(new BrUpdateInfo)
        val rollback = Input(Bool())

    })

    val rtype = RT_FIX

    def FindAndCompareTaints(
            uop: MicroOp, 
            older_uops: Seq[MicroOp], 
            taint_entries: Seq[TaintEntry]) 
            : TaintEntry = {
        val matching_uop = Wire(new MicroOp)
        matching_uop := uop

        val uop_ass_ldst = older_uops map { case (u) => u.ldst_val && u.dst_rtype === rtype}

        val bypass_hits_rs1 = (older_uops zip uop_ass_ldst) map { case (r, a) => a && r.ldst === uop.lrs1}
        val bypass_hits_rs2 = (older_uops zip uop_ass_ldst) map { case (r, a) => a && r.ldst === uop.lrs2}
        val bypass_hits_rs3 = (older_uops zip uop_ass_ldst) map { case (r, a) => a && r.ldst === uop.lrs3} 

        val bypass_sel_rs1 = PriorityEncoderOH(bypass_hits_rs1.reverse).reverse
        val bypass_sel_rs2 = PriorityEncoderOH(bypass_hits_rs2.reverse).reverse
        val bypass_sel_rs3 = PriorityEncoderOH(bypass_hits_rs3.reverse).reverse

        val do_bypass_rs1 = bypass_hits_rs1.reduce(_||_)
        val do_bypass_rs2 = bypass_hits_rs2.reduce(_||_)
        val do_bypass_rs3 = bypass_hits_rs3.reduce(_||_)

        val t1 = Wire(new TaintEntry)
        val t2 = Wire(new TaintEntry)
        val t3 = Wire(new TaintEntry)

        t1 := Mux(do_bypass_rs1, Mux1H(bypass_sel_rs1, taint_entries), taint_file(matching_uop.lrs1))
        t2 := Mux(do_bypass_rs2, Mux1H(bypass_sel_rs2, taint_entries), taint_file(matching_uop.lrs2))
        t3 := Mux(do_bypass_rs3, Mux1H(bypass_sel_rs3, taint_entries), taint_file(matching_uop.lrs3))

        GetTaintEntry(matching_uop, t1, t2, t3)

    }

    def GetTaintEntry(
        uop: MicroOp,
        t1: TaintEntry,
        t2: TaintEntry,
        t3: TaintEntry)
        : TaintEntry = {

        val t1_valid = t1.valid
        val t2_valid = t2.valid
        val t3_valid = t3.valid

        val t1_age = t1.age 
        val t1_flipped = t1.flipped_age
        val t2_age = t2.age 
        val t2_flipped = t2.flipped_age
        val t3_age = t3.age 
        val t3_flipped = t3.flipped_age

        val any_valid = t1_valid || t2_valid || t3_valid

        val t1_youngness = Mux(io.ldq_flipped === t1_flipped, 256.U - t1_age, 512.U - t1_age)
        val t2_youngness = Mux(io.ldq_flipped === t2_flipped, 256.U - t2_age, 512.U - t2_age)
        val t3_youngness = Mux(io.ldq_flipped === t3_flipped, 256.U - t3_age, 512.U - t3_age)
        
        val t1_oldest = (t1_valid 
                        && ((!t2_valid) || t1_youngness < t2_youngness)
                        && ((!t3_valid) || t1_youngness < t3_youngness))
        val t2_oldest = ((!t1_oldest)
                        && t2_valid
                        && ((!t3_valid) || t2_youngness < t3_youngness))
        val t3_oldest = ((!t1_oldest)
                        && (!t2_oldest)
                        && t3_valid) 
        

        when(!any_valid) {
            val f_entry = Wire(new TaintEntry())
            f_entry.valid := false.B
            f_entry.ldq_idx := 0.U
            f_entry.age := 0.U
            f_entry.flipped_age := false.B
        }.elsewhen(t1_oldest) {
            t1
        }.elsewhen(t2_oldest) {
            t2
        }

        t3
    }

    val ren1_uops = Wire(Vec(plWidth, new MicroOp()))
    val ren1_fire = Wire(Vec(plWidth, new Bool()))

    ren1_uops := io.ren1_uops
    ren1_fire := io.ren1_fire

    val taint_file = Reg(Vec(numLregs, new TaintEntry()))

    val br_snapshots = Reg(Vec(maxBrCount, Vec(numLregs, new TaintEntry())))
    val br_remap_table  = Wire(Vec(plWidth+1, Vec(numLregs, new TaintEntry())))

    val new_taint_entries = Wire(Vec(plWidth, new TaintEntry()))


    for (i <- 0 until plWidth) {
        val t_ent = Wire(new TaintEntry())
        if (i > 0) t_ent := FindAndCompareTaints(ren1_uops(i), 
                                                 ren1_uops.slice(0, i),
                                                 new_taint_entries.slice(0, i))
        else       t_ent := GetTaintEntry(ren1_uops(i), 
                                          taint_file(ren1_uops(i).lrs1),
                                          taint_file(ren1_uops(i).lrs2),
                                          taint_file(ren1_uops(i).lrs3))
        
        ren1_uops(i).yrot := t_ent.ldq_idx

        when(ren1_fire(i) && ren1_uops(i).uses_ldq) {
            t_ent.ldq_idx := ren1_uops(i).ldq_idx
            t_ent.age := ren1_uops(i).ldq_idx
            t_ent.valid := true.B
            new_taint_entries(i) := t_ent
        }. elsewhen(ren1_fire(i)) {
            new_taint_entries(i) := t_ent
        }. otherwise {
            t_ent.valid := false.B
            new_taint_entries(i) := t_ent
        }
        
    }

    for (i <- 0 until numLregs) {
        val remapped_entry = (ren1_uops.map(uop => (uop.ldst_val, uop.ldst)) zip ren1_fire zip new_taint_entries)
            .scanLeft(taint_file(i)) {case (t_ent, (((ldst_val, ldst), fire), new_t_ent)) => Mux(fire && ldst_val && ldst === i.U, new_t_ent, t_ent)}
        
        for (j <- 0 until plWidth+1) {
            br_remap_table(j)(i) := remapped_entry(j)
        }
    }

    for (i <- 0 until plWidth) {
        when(io.ren_br_tags(i).valid) {
            br_snapshots(io.ren_br_tags(i).bits) := br_remap_table(i+1)
        }
    }

    when (io.brupdate.b2.mispredict) {
        taint_file := br_snapshots(io.brupdate.b2.uop.br_tag)
    } .otherwise {
        taint_file := br_remap_table(plWidth)
    }

    for (i <- 0 until plWidth) {
        io.uops_yrot(i) := ren1_uops(i).ldq_idx
    }

    dontTouch(taint_file)
    dontTouch(br_snapshots)
    dontTouch(br_remap_table)
    dontTouch(io)

}