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

    //The io bundle inputs are now identical to that of the rename stage
    val io = IO(new Bundle {
        val stt_stalls = Output(Vec(plWidth, Bool()))

        val kill = Input(Bool())

        val dec_fire = Input(Vec(plWidth, Bool()))
        val dec_uops = Input(Vec(plWidth, new MicroOp()))

        val ldq_tail = Input(UInt(ldqAddrSz.W))

        val brupdate = Input(new BrUpdateInfo())

        val ren2_yrot = Output(Vec(plWidth, (UInt(ldqAddrSz.W))))
        val ren2_valid = Output(Vec(plWidth, Bool()))

        val dis_fire = Input(Vec(coreWidth, Bool()))
        val dis_ready = Input(Bool())

        val com_valids = Input(Vec(plWidth, Bool()))
        val com_uops = Input(Vec(plWidth, new MicroOp()))
        val rbk_valids = Input(Vec(plWidth, Bool()))
        val rollback = Input(Bool())

        val ldq_flipped = Input(Bool())
    })

    val int_type = RT_FIX
    val fp_type = RT_FLT
    val dud_entry = Wire(new TaintEntry())
    dud_entry.valid := false.B
    dud_entry.ldq_idx := 0.U
    dud_entry.age := 0.U
    dud_entry.flipped_age := false.B

    def FindAndCompareTaints(
            uop: MicroOp, 
            older_uops: Seq[MicroOp], 
            taint_entries: Seq[TaintEntry]) 
            : TaintEntry = {
        val matching_uop = Wire(new MicroOp)
        matching_uop := uop

        val bypass_hits_rs1 = older_uops map {case r => r.ldst_val && r.dst_rtype === uop.lrs1_rtype && r.ldst === uop.lrs1}
        val bypass_hits_rs2 = older_uops map {case r => r.ldst_val && r.dst_rtype === uop.lrs2_rtype && r.ldst === uop.lrs2}
        val bypass_hits_rs3 = older_uops map {case r => r.ldst_val && r.dst_rtype === fp_type && uop.frs3_en && r.ldst === uop.lrs3}

        val bypass_sel_rs1 = PriorityEncoderOH(bypass_hits_rs1.reverse).reverse
        val bypass_sel_rs2 = PriorityEncoderOH(bypass_hits_rs2.reverse).reverse
        val bypass_sel_rs3 = PriorityEncoderOH(bypass_hits_rs3.reverse).reverse

        val do_bypass_rs1 = bypass_hits_rs1.reduce(_||_)
        val do_bypass_rs2 = bypass_hits_rs2.reduce(_||_)
        val do_bypass_rs3 = bypass_hits_rs3.reduce(_||_)

        val t1 = Wire(new TaintEntry)
        val t2 = Wire(new TaintEntry)
        val t3 = Wire(new TaintEntry)

        val (d_t1, d_t2, d_t3) = GetRegTaints(uop)

        t1 := Mux(do_bypass_rs1, Mux1H(bypass_sel_rs1, taint_entries), d_t1)
        t2 := Mux(do_bypass_rs2, Mux1H(bypass_sel_rs2, taint_entries), d_t2)
        t3 := Mux(do_bypass_rs3, Mux1H(bypass_sel_rs3, taint_entries), d_t3)

        GetTaintEntry(matching_uop, t1, t2, t3)

    }

    def GetTaintEntry(
        uop: MicroOp,
        t1: TaintEntry,
        t2: TaintEntry,
        t3: TaintEntry)
        : TaintEntry = {

        val rs3_en = uop.frs3_en

        val t1_valid = t1.valid
        val t2_valid = t2.valid
        val t3_valid = t3.valid && rs3_en

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
        chosen_t.ldq_idx := 31.U
        chosen_t.age := 13.U
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

    def GetRegTaints(uop: MicroOp) : (TaintEntry, TaintEntry, TaintEntry) = {

        val t1 = MuxLookup(uop.lrs1_rtype, dud_entry,
                    Array(int_type -> int_taint_file(uop.lrs1),
                          fp_type -> fp_taint_file(uop.lrs1)))

        val t2 = MuxLookup(uop.lrs2_rtype, dud_entry,
                    Array(int_type -> int_taint_file(uop.lrs2),
                          fp_type -> fp_taint_file(uop.lrs2)))

        val t3 = Mux(uop.frs3_en, fp_taint_file(uop.lrs3), dud_entry)
        
        (t1, t2, t3)
    }

    val load_ops = ((io.dec_fire zip io.dec_uops) map { case (f, u) => (f && u.uses_ldq)})
                    .scanLeft(0.U(4.W))((total, load) => total + load.asUInt())
    val loads_last_cycle = RegNext(load_ops.last)

    val int_taint_file = Reg(Vec(numLregs, new TaintEntry()))
    val fp_taint_file = Reg(Vec(numLregs, new TaintEntry()))

    val int_br_snapshots = Reg(Vec(maxBrCount, Vec(numLregs, new TaintEntry())))
    val fp_br_snapshots = Reg(Vec(maxBrCount, Vec(numLregs, new TaintEntry())))

    val int_br_remap_table  = Wire(Vec(plWidth+1, Vec(numLregs, new TaintEntry())))
    val fp_br_remap_table  = Wire(Vec(plWidth+1, Vec(numLregs, new TaintEntry())))

    val ren1_uops = Wire(Vec(plWidth, new MicroOp()))
    val ren1_fire = Wire(Vec(plWidth, Bool()))

    val ren1_br_tags = Wire(Vec(plWidth, Valid(UInt(brTagSz.W))))

    dontTouch(ren1_uops)
    dontTouch(ren1_fire)

    val ren2_fire       = io.dis_fire
    val ren2_ready      = io.dis_ready
    val ren2_valids     = Wire(Vec(plWidth, Bool()))
    val ren2_uops       = Wire(Vec(plWidth, new MicroOp))

    for (w <- 0 until plWidth) {
        ren1_fire(w) := io.dec_fire(w)
        ren1_uops(w) := io.dec_uops(w)
    }

    for (w <- 0 until plWidth) {
        ren1_br_tags(w).valid := ren1_fire(w) && ren1_uops(w).allocate_brtag
        ren1_br_tags(w).bits := ren1_uops(w).br_tag
    }

    for (w <- 0 until plWidth) {
        val r_valid = RegInit(false.B)
        val r_uop = Reg(new MicroOp)
        val next_uop = Wire(new MicroOp)

        next_uop := r_uop

        when (io.kill) {
            r_valid := false.B
        }.elsewhen (ren2_ready) {
            r_valid := ren1_fire(w)
            next_uop := ren1_uops(w)
        }.otherwise {
            r_valid := r_valid && !ren2_fire(w)
            next_uop := r_uop
        }
        ren2_valids(w) := r_valid
        ren2_uops(w) := r_uop
        dontTouch(next_uop)
    }
    val new_taint_entries = Wire(Vec(plWidth, new TaintEntry()))

    for (i <- 0 until plWidth) {
        val t_ent = Wire(new TaintEntry())

        val (t1, t2, t3) = GetRegTaints(ren1_uops(i))


        if (i > 0) {
            t_ent := FindAndCompareTaints(ren1_uops(i), 
                                          ren1_uops.slice(0, i),
                                          new_taint_entries.slice(0, i))
        } else {
            t_ent := GetTaintEntry(ren1_uops(i), t1, t2, t3)
        }

        ren1_uops(i).yrot := t_ent.ldq_idx

        when(ren1_fire(i) && ren1_uops(i).uses_ldq) {
            t_ent.ldq_idx := WrapAdd(io.ldq_tail, loads_last_cycle + load_ops(i), numLdqEntries) //ren1_uops(i).ldq_idx
            t_ent.age := WrapAdd(io.ldq_tail, loads_last_cycle +  load_ops(i), numLdqEntries) //ren1_uops(i).ldq_idx
            t_ent.valid := true.B
            new_taint_entries(i) := t_ent
        }. elsewhen(ren1_fire(i)) {
            new_taint_entries(i) := t_ent
        }. otherwise {
            t_ent.valid := false.B
            new_taint_entries(i) := t_ent
        }   
    }

    // Int update
    for (i <- 0 until numLregs) {
        val remapped_int_entry = (ren1_uops.map(uop => (uop.ldst_val, uop.ldst, uop.dst_rtype)) zip ren1_fire zip new_taint_entries)
            .scanLeft(int_taint_file(i)) {case (t_ent, (((ldst_val, ldst, rtype), fire), new_t_ent)) => Mux(fire && ldst_val && ldst === i.U && rtype === int_type, new_t_ent, t_ent)}
        
        for (j <- 0 until plWidth+1) {
            int_br_remap_table(j)(i) := remapped_int_entry(j)
        }
    }

    // Float update
    for (i <- 0 until numLregs) {
        val remapped_float_entry = (ren1_uops.map(uop => (uop.ldst_val, uop.ldst, uop.dst_rtype)) zip ren1_fire zip new_taint_entries)
            .scanLeft(fp_taint_file(i)) {case (t_ent, (((ldst_val, ldst, rtype), fire), new_t_ent)) => Mux(fire && ldst_val && ldst === i.U && rtype === fp_type, new_t_ent, t_ent)}
        
        for (j <- 0 until plWidth+1) {
            fp_br_remap_table(j)(i) := remapped_float_entry(j)
        }
    }


    for (i <- 0 until plWidth) {
        when(ren1_br_tags(i).valid) {
            int_br_snapshots(ren1_br_tags(i).bits) := int_br_remap_table(i+1)
            fp_br_snapshots(ren1_br_tags(i).bits) := fp_br_remap_table(i+1)
        }
    }
    when (io.brupdate.b2.mispredict) {
        int_taint_file := int_br_snapshots(io.brupdate.b2.uop.br_tag)
        fp_taint_file := fp_br_snapshots(io.brupdate.b2.uop.br_tag)
    } .otherwise {
        int_taint_file := int_br_remap_table(plWidth)
        fp_taint_file := fp_br_remap_table(plWidth)
    }

    for (w <- 0 until plWidth) {
        io.ren2_yrot(w) := ren2_uops(w).yrot
        io.ren2_valid(w) := ren2_valids(w)
    }

    dontTouch(int_taint_file)
    dontTouch(fp_taint_file)
    dontTouch(int_br_snapshots)
    dontTouch(fp_br_snapshots)
    dontTouch(int_br_remap_table)
    dontTouch(fp_br_remap_table)
    dontTouch(io)
    dontTouch(new_taint_entries)

}