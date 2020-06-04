ROB - 115:
// --------------------------------------------------------------------
  // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  // TODO: Make this into a ShadowBufferInterface type
  val sb_tail = Input(UInt(sbAddrSz.W)) //Probably not needed
  val sb_head = Input(UInt(sbAddrSz.W)) //Probably not needed
  val sb_q_idx = Input(Vec(coreWidth, UInt(sbAddrSz.W)))

  val sb_enq = Output(Vec(coreWidth,Valid(new MicroOp())))
  val sb_wb_uop = Output(Vec(numWakeupPorts, Valid(UInt(sbAddrSz.W))))
  val sb_rbk = Output(Vec(coreWidth, Valid(UInt(sbAddrSz.W))))
  val sb_full = Input(Bool())
  val sb_empty = Input(Bool())

  val rq_enq = Output(Vec(coreWidth, new RQEnqSignals()))
  val rq_full = Input(Bool())
  // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  //----------------------------------------------------------------------

ROB - 313:
  // --------------------------------------------------------------------
  // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  for (i <- 0 until numWakeupPorts){ io.sb_wb_uop(i).valid :=false.B}
  for (i <- 0 until coreWidth) {io.sb_rbk(i).valid := false.B}
  val rob_bank_enq_sb = WireInit(VecInit(Seq.fill(coreWidth){false.B})) // for multi-core
  // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  //----------------------------------------------------------------------

ROB - 331:
// --------------------------------------------------------------------
    // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
    val rob_sb_val    = RegInit(VecInit(Seq.fill(numRobRows){false.B}))
    val rob_sb_idx    = Reg(Vec(numRobRows, UInt(sbAddrSz.W)))

    dontTouch(io.sb_enq)
    dontTouch(io.sb_wb_uop)
    dontTouch(io.sb_q_idx)

    //-----------------------------------------------
    // Dispatch: Add Entry to ROB

    /*erlingrj default false for enque to Shadow Buffer and Release Queue */
    io.sb_enq(w).valid := false.B
    io.rq_enq(w).valid := false.B
    // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
    //----------------------------------------------------------------------

ROB - 362:
// --------------------------------------------------------------------
      // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
      /* erlingrj 17/9 */
      when(io.enq_uops(w).has_c_shadow) {
         io.sb_enq(w).valid   := true.B
         io.sb_enq(w).bits    := io.enq_uops(w)
         rob_sb_val(rob_tail) := true.B
         rob_sb_idx(rob_tail) := io.sb_q_idx(w)
         rob_bank_enq_sb(w)   := true.B
      }.otherwise {
        io.sb_enq(w).valid := false.B
        rob_sb_val(rob_tail) := false.B
      }

      when(io.enq_uops(w).is_load) {
        //Now we have to watch out. If we have core>0 then we need to check if
        // the other cores enqueued something in this CC. Then the latest
        // of those unsafe instr is the one shadowing this load
        if (coreWidth == 1) {
          when(!io.sb_empty) {
            io.rq_enq(w).valid := true.B
            io.rq_enq(w).ldq_idx := io.enq_uops(w).ldq_idx
            io.rq_enq(w).sb_idx := io.sb_tail
          }
        } else {
          when(rob_bank_enq_sb.zipWithIndex.map({ case (value, idx) =>
                                                  if (idx < w) value
                                                  else false.B}).reduce((a,b) => (a|b)))
          { // If a new instruction was queued to SB this CC that was BEFORE this load in program order

            for (i <- 0 until w) {
              when(rob_bank_enq_sb(i)) {
                io.rq_enq(w).valid := true.B
                io.rq_enq(w).ldq_idx := io.enq_uops(w).ldq_idx
                io.rq_enq(w).sb_idx := io.sb_q_idx(i)
              }
            }
          }.otherwise {
            when(!io.sb_empty) { //TODO: What if the only entry is a "killed" one?
              io.rq_enq(w).valid := true.B
              io.rq_enq(w).ldq_idx := io.enq_uops(w).ldq_idx
              io.rq_enq(w).sb_idx := io.sb_tail
            }

          }
        }
      }
      // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
      //----------------------------------------------------------------------

ROB - 432:
// --------------------------------------------------------------------
        // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
        when(rob_sb_val(row_idx)) {
           io.sb_wb_uop(i).bits := rob_sb_idx(row_idx)
           io.sb_wb_uop(i).valid := true.B
           rob_sb_val(row_idx) := false.B
        }

        // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
        //----------------------------------------------------------------------


ROB - 534:
// --------------------------------------------------------------------
      // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
      /*erlingrj: when rolling back, also kill the associated ShadowBuffer entry */
      when(rob_sb_val(com_idx)) {
        io.sb_rbk(w).valid := true.B
        io.sb_rbk(w).bits := rob_sb_idx(com_idx)
      }
      // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
      //----------------------------------------------------------------------


CORE - 145:
  // ---------------------------------------------------------------------------------------
  // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  /*erlingrj 2/9 */
  val sb               = Module(new ShadowBuffer(
                                 numIrfWritePorts + 1 + numFpWakeupPorts)) //TODO: WHY THIS? erlingrj 17. september
  val rq               = Module(new ReleaseQueue(
                                coreWidth,
  ))
  // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  //----------------------------------------------------------------------------------------

CORE - 242:
  // --------------------------------------------------------------------
  // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
    ("Shadowed load stalled",     () => lsu.io.perf_shadow_stall),
    ("loads sent to D$",          () => lsu.io.counters.ld_valid),
    ("Shadow Buffer stalls",      () => sb.io.full),
    ("Release Queue stalls",      () => rq.io.full),
    ("LSU stalls",                () => lsu.io.laq_full(0)), //TODO: Fix the laq_full. Whats up with pl_width
    ("ROB stalls",                () => !rob.io.ready))),

    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("LSU kills",                 () => lsu.io.xcpt.valid),
      ("LSU ld-ld order fail",      () => lsu.io.counters.ld_killed),
      ("D$ miss",                   () => !lsu.io.memreq_val),
      ("branch misprediction",      () => br_unit.brinfo.mispredict),
      ("branch resolved",           () => br_unit.brinfo.valid),
      ("flush",                     () => rob.io.commit.rollback)))
    // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  //----------------------------------------------------------------------

CORE - 638:
// ---------------------------------------------------------------------------------------
  // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no

  //-------------------------------------------------------------
  // ShadowBuffer and ReleaseQueue
  /*erlingrj 2/9 Connect ShadowBuffer and ROB*/
  rob.io.sb_tail := sb.io.tail_spec
  rob.io.sb_head := sb.io.head
  rob.io.sb_full := sb.io.full
  rob.io.sb_q_idx := sb.io.q_idx
  rob.io.sb_empty := sb.io.empty
  rob.io.rq_full := rq.io.full

  sb.io.enq_uop <> rob.io.sb_enq
  sb.io.wb_uop <> rob.io.sb_wb_uop
  sb.io.brinfo <> br_unit.brinfo
  sb.io.rollback <> rob.io.sb_rbk

  rq.io.commit := sb.io.release
  rq.io.enq := rob.io.rq_enq
  rq.io.exception := rob.io.flush.valid


  lsu.io.set_shadow_bit := rq.io.set_shadow_bit
  lsu.io.unset_shadow_bit := rq.io.unset_shadow_bit

  // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  //----------------------------------------------------------------------------------------

CORE - 817:
// ---------------------------------------------------------------------------------------
  // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  // TODO: Write description

  val iss_loadIssued =
    mem_iq.io.iss_valids(0) &&
    mem_iq.io.iss_uops(0).is_load &&
    !mem_iq.io.iss_uops(0).fp_val &&
    mem_iq.io.iss_uops(0).pdst =/= 0.U &&
    !(sxt_ldMiss && (mem_iq.io.iss_uops(0).iw_p1_poisoned || mem_iq.io.iss_uops(0).iw_p2_poisoned))
  sxt_ldMiss :=
    ((lsu.io.nack.valid && lsu.io.nack.isload) || dc_shim.io.core.load_miss) &&
      (Pipe(true.B, iss_loadIssued, 4).bits &&
      !lsu.io.incoming_load_was_shadowed_and_no_spec_wakeup &&
      !lsu.io.incoming_load_was_shadowed_and_nonspec_wakeup) ||
      RegNext(lsu.io.mem_ldSpecWakeup.valid) //Add this

  issue_units.map(_.io.sxt_ldMiss := sxt_ldMiss)
  // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
  //----------------------------------------------------------------------------------------

LSU - 158:
// ---------------------------------------------------------------------------------------
   // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   // TODO: Write description
   val set_shadow_bit = Input(Vec(pl_width, Flipped(Valid(UInt(ldqAddrSz.W)))))
   val unset_shadow_bit = Input(Vec(rqCommitWidth, Flipped(Valid(UInt(ldqAddrSz.W)))))
   val incoming_load_was_shadowed_and_nonspec_wakeup = Output(Bool())
   val incoming_load_was_shadowed_and_no_spec_wakeup = Output(Bool()) // If incoming load in prev CC was shadowed == cache nack
   val perf_shadow_stall = Output(Bool()) // For performance counter
   val perf_n_loads_sent_to_dmem = Output(Bool()) // How many loads are sent to D$
   // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   //----------------------------------------------------------------------------------------


LSU - 227:
// ---------------------------------------------------------------------------------------
   // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   /* erlingrj support shadowing of loads */                                                                                             // for this ID to wakeup
   val laq_is_shadowed        = Reg(Vec(NUM_LDQ_ENTRIES, Bool())) // load is shadowed and can NOT be fired to memory// can only be woken up when the ReleaseQueue has
                                                                  // unset this bit
   dontTouch(io.set_shadow_bit)
   dontTouch(io.unset_shadow_bit)
   // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   //----------------------------------------------------------------------------------------

LSU - 272:
    // TODO move this to bottom of file
   // ---------------------------------------------------------------------------------------
   // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   /*erlingrj: Flip is_shadowed bits when Release Queue gives notice  */
   for (w <- 0 until rqCommitWidth)
   {
      when (io.unset_shadow_bit(w).valid)
      {
         val ldq_idx = io.unset_shadow_bit(w).bits
         laq_is_shadowed(ldq_idx) := false.B

         assert(laq_allocated(ldq_idx), "[lsu] RQ tries to unset an invalid load, ldq_idx=%d",ldq_idx)
         assert(!laq_executed(ldq_idx), "[lsu] RQ tries to unset an executed load, ldq_idx=%d", ldq_idx)

      }

   }
   // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   //----------------------------------------------------------------------------------------

LSU - 337:
// ---------------------------------------------------------------------------------------
         // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
         /*erlingrj for now, just set it to false */
         // TODO interface to ReleaseQueue
         when(io.set_shadow_bit(w).valid) {
            laq_is_shadowed(ld_enq_idx) := true.B
            assert(io.set_shadow_bit(w).bits === ld_enq_idx, "[lsu] mismatch between rq_set_shadow_bit and ldq_idx. ")
         }.otherwise {
            laq_is_shadowed(ld_enq_idx) := false.B
         }
         // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
         //----------------------------------------------------------------------------------------


LSU - 408:
   // ---------------------------------------------------------------------------------------
   // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
    /* erlingrj for shadowed loads
   If we receive the calculated address for a shadowed load, we wish to do the address translation
   but then just put the load to sleep. This will allow a load_wakeup to happen simultaneously
    */
   val will_fire_shadowed_load_incoming   = WireInit(false.B) // uses TLB
   // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   //----------------------------------------------------------------------------------------

LSU - 720:
// ---------------------------------------------------------------------------------------
   // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   /* erlingrj: Write the shadowed load into the LAQ*/
   // TODO move this together with will_fire_load and retry
   when(will_fire_shadowed_load_incoming)
   {
      laq_addr_val      (exe_tlb_uop.ldq_idx)      := true.B
      laq_addr          (exe_tlb_uop.ldq_idx)      := Mux(tlb_miss, exe_vaddr, exe_tlb_paddr)
      laq_uop           (exe_tlb_uop.ldq_idx).pdst := exe_tlb_uop.pdst
      laq_is_virtual    (exe_tlb_uop.ldq_idx)      := tlb_miss
      laq_is_uncacheable(exe_tlb_uop.ldq_idx)      := tlb_addr_uncacheable && !tlb_miss
      assert(laq_is_shadowed(exe_tlb_uop.ldq_idx), "[lsu] incoming load put to sleep after TLB but is not shadowed (anymore?)")
   }
   // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   //----------------------------------------------------------------------------------------

LSU - 822:
 // ---------------------------------------------------------------------------------------
   // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   // Here we need to accomodate some BOOM quirks. The Issue Unit speculates that all loads will
   // return in 4 CC with a result. mem_ldS

   io.mem_ldSpecWakeup.valid := RegNext((will_fire_load_incoming
     && !io.exe_resp.bits.uop.fp_val
     && io.exe_resp.bits.uop.pdst =/= 0.U) ||
     (will_fire_shadowed_load_incoming && will_fire_load_wakeup
       && !exe_ld_uop.fp_val
       && exe_ld_uop.pdst =/= 0.U)
      ,init=false.B)

   io.mem_ldSpecWakeup.bits := mem_ld_uop.pdst
   // We also have to accomodate 2 corner cases. The Issue Unit will speculatively dispatch
   //    instructions depending on loads assuming the loads fire straight of the mem
   //    When this instructions are shadowed
   //    blabla to complicated to describe here
   io.incoming_load_was_shadowed_and_no_spec_wakeup := RegNext(RegNext((will_fire_shadowed_load_incoming && !will_fire_load_wakeup)))
   io.incoming_load_was_shadowed_and_nonspec_wakeup := RegNext(RegNext(will_fire_shadowed_load_incoming
     && will_fire_load_wakeup
     && (exe_ld_uop.fp_val || exe_ld_uop.pdst === 0.U)))

   // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   //----------------------------------------------------------------------------------------

LSU - 1475:
   // Begin: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   // Performance counters
   io.perf_shadow_stall := will_fire_shadowed_load_incoming
   io.perf_n_loads_sent_to_dmem := io.memreq_val && io.memreq_uop.is_load

   // End: Eager Delay for speculative loads by erlingrj@stud.ntnu.no
   //----------------------------------------------------------------------------------------
