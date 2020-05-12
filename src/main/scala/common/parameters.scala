//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.common

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.subsystem.MemoryPortParams
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink.{BootROMParams, CLINTParams, PLICParams}
import boom.ifu._
import boom.bpu._
import boom.exu._
import boom.lsu._
import lsc.Hash

/**
 * Default BOOM core parameters
 */
case class BoomCoreParams(
// DOC include start: BOOM Parameters
  fetchWidth: Int = 1,
  decodeWidth: Int = 1,
  numRobEntries: Int = 64,
  issueParams: Seq[IssueParams] = Seq(
    IssueParams(issueWidth=1, numEntries=16, iqType=IQT_MEM.litValue, dispatchWidth=1),
    IssueParams(issueWidth=2, numEntries=16, iqType=IQT_INT.litValue, dispatchWidth=1),
    IssueParams(issueWidth=1, numEntries=16, iqType=IQT_FP.litValue , dispatchWidth=1)),
  numLdqEntries: Int = 16,
  numStqEntries: Int = 16,
  numIntPhysRegisters: Int = 96,
  numFpPhysRegisters: Int = 64,
  maxBrCount: Int = 4,
  numFetchBufferEntries: Int = 16,
  enableAgePriorityIssue: Boolean = true,
  enablePrefetching: Boolean = false,
  enableFastLoadUse: Boolean = true,
  enableCommitMapTable: Boolean = false,
  enableFastPNR: Boolean = false,
  enableBTBContainsBranches: Boolean = true,
  enableBranchPredictor: Boolean = true,
  enableBTB: Boolean = true,
  enableBpdUModeOnly: Boolean = false,
  enableBpdUSModeHistory: Boolean = false,
  useAtomicsOnlyForIO: Boolean = false,
  ftq: FtqParameters = FtqParameters(),
  btb: BoomBTBParameters = BoomBTBParameters(),
  bim: BimParameters = BimParameters(),
  tage: Option[TageParameters] = None,
  gshare: Option[GShareParameters] = None,
  bpdBaseOnly: Option[BaseOnlyParameters] = None,
  bpdRandom: Option[RandomBpdParameters] = None,
  intToFpLatency: Int = 2,
  imulLatency: Int = 3,
  nPerfCounters: Int = 0,
  numRXQEntries: Int = 4,
  numRCQEntries: Int = 8,
  numDCacheBanks: Int = 1,
  nPMPs: Int = 8,
  /* more stuff */

  useFetchMonitor: Boolean = true,
  bootFreqHz: BigInt = 0,
  fpu: Option[FPUParams] = Some(FPUParams(sfmaLatency=4, dfmaLatency=4)),
  usingFPU: Boolean = true,
  haveBasicCounters: Boolean = true,
  misaWritable: Boolean = false,
  mtvecInit: Option[BigInt] = Some(BigInt(0)),
  mtvecWritable: Boolean = true,
  haveCFlush: Boolean = false,
  mulDiv: Option[freechips.rocketchip.rocket.MulDivParams] = Some(MulDivParams(divEarlyOut=true)),
  nBreakpoints: Int = 0, // TODO Fix with better frontend breakpoint unit
  nL2TLBEntries: Int = 512,
  nLocalInterrupts: Int = 0,
  useAtomics: Boolean = true,
  useDebug: Boolean = true,
  useUser: Boolean = true,
  useVM: Boolean = true,
  useCompressed: Boolean = false,
  useSCIE: Boolean = false,
  useRVE: Boolean = false,
  useBPWatch: Boolean = false,
  clockGate: Boolean = false,
  loadSliceCore: Option[LoadSliceCoreParams] = None,
  ibdaParams: Option[IbdaParams] = None,
  busyLookupParams: Option[BusyLookupParams] = None,
  dnbParams: Option[DnbParams] = None,
  casParams: Option[CasParams] = None,
inoParams: Option[InoParams] = None

) extends freechips.rocketchip.tile.CoreParams
{
  val haveFSDirty = true
  val pmpGranularity: Int = 4
  val instBits: Int = if (useCompressed) 16 else 32
  val lrscCycles: Int = 80 // worst case is 14 mispredicted branches + slop
  val retireWidth = decodeWidth
  val jumpInFrontend: Boolean = false // unused in boom
  val loadSliceMode: Boolean = loadSliceCore.isDefined
  val dnbMode: Boolean = dnbParams.isDefined
  val casMode: Boolean = casParams.isDefined
  val inoMode: Boolean = inoParams.isDefined
  val ibdaMode: Boolean = loadSliceMode || dnbMode
  val busyLookupMode: Boolean = loadSliceMode || dnbMode || casMode

  // Make sure we have enough lookup ports to the Busy Table
  if(busyLookupMode) {
    if (loadSliceMode) {
      require(busyLookupParams.get.lookupAtDisWidth == loadSliceCore.get.dispatches())
    }
    if (dnbMode) {
      require(busyLookupParams.get.lookupAtDisWidth == dnbParams.get.dlqDispatches)
    }

    if (casMode) {
      require(busyLookupParams.get.lookupAtDisWidth == casParams.get.inqDispatches + casParams.get.windowSize)
    }
  }


  val unifiedIssueQueue: Boolean = loadSliceCore.exists(_.unifiedIssueQueue) || dnbMode || casMode || inoMode

  override def customCSRs(implicit p: Parameters) = new BoomCustomCSRs
}

/**
  * Defines custom BOOM CSRs
  */
class BoomCustomCSRs(implicit p: Parameters) extends freechips.rocketchip.tile.CustomCSRs
  with HasBoomCoreParameters {
  override def chickenCSR = {
    val params = tileParams.core.asInstanceOf[BoomCoreParams]
    val mask = BigInt(
      tileParams.dcache.get.clockGate.toInt << 0 |
      params.clockGate.toInt << 1 |
      params.clockGate.toInt << 2 |
      1 << 3 // Disable OOO when this bit is high
    )
    val init = BigInt(
      tileParams.dcache.get.clockGate.toInt << 0 |
      params.clockGate.toInt << 1 |
      params.clockGate.toInt << 2 |
      0 << 3 // Enable OOO at init
    )
    Some(CustomCSR(chickenCSRId, mask, Some(init)))
  }
  def disableOOO = getOrElse(chickenCSR, _.value(3), true.B)
}

/**
 * Mixin trait to add BOOM parameters to expand other traits/objects/etc
 */
trait HasBoomCoreParameters extends freechips.rocketchip.tile.HasCoreParameters
{
  val boomParams: BoomCoreParams = tileParams.core.asInstanceOf[BoomCoreParams]

  //************************************
  // Superscalar Widths

  // fetchWidth provided by CoreParams class.
  // decodeWidth provided by CoreParams class.

  // coreWidth is width of decode, width of integer rename, width of ROB, and commit width
  val coreWidth = decodeWidth

  require (isPow2(fetchWidth))
  require (coreWidth <= fetchWidth)

  //************************************
  // Data Structure Sizes
  val numRobEntries = boomParams.numRobEntries       // number of ROB entries (e.g., 32 entries for R10k)
  val numRxqEntries = boomParams.numRXQEntries       // number of RoCC execute queue entries. Keep small since this holds operands and instruction bits
  val numRcqEntries = boomParams.numRCQEntries       // number of RoCC commit queue entries. This can be large since it just keeps a pdst
  val numLdqEntries = boomParams.numLdqEntries       // number of LAQ entries
  val numStqEntries = boomParams.numStqEntries       // number of SAQ/SDQ entries
  val maxBrCount    = boomParams.maxBrCount          // number of branches we can speculate simultaneously
  val ftqSz         = boomParams.ftq.nEntries        // number of FTQ entries
  val numFetchBufferEntries = boomParams.numFetchBufferEntries // number of instructions that stored between fetch&decode

  val numIntPhysRegs= boomParams.numIntPhysRegisters // size of the integer physical register file
  val numFpPhysRegs = boomParams.numFpPhysRegisters  // size of the floating point physical register file

  //************************************
  // Functional Units
  val usingFDivSqrt = boomParams.fpu.isDefined && boomParams.fpu.get.divSqrt

  val mulDivParams = boomParams.mulDiv.getOrElse(MulDivParams())
  // TODO: Allow RV32IF
  require(!(xLen == 32 && usingFPU), "RV32 does not support fp")

  //************************************
  // Pipelining

  val imulLatency = boomParams.imulLatency
  val dfmaLatency = if (boomParams.fpu.isDefined) boomParams.fpu.get.dfmaLatency else 3
  val sfmaLatency = if (boomParams.fpu.isDefined) boomParams.fpu.get.sfmaLatency else 3
  // All FPU ops padded out to same delay for writeport scheduling.
  require (sfmaLatency == dfmaLatency)

  val intToFpLatency = boomParams.intToFpLatency

  //************************************
  // Issue Units

  val issueParams: Seq[IssueParams] = boomParams.issueParams
  val enableAgePriorityIssue = boomParams.enableAgePriorityIssue

  // currently, only support one of each.
  require (issueParams.count(_.iqType == IQT_FP.litValue) == 1 || !usingFPU)
  require (issueParams.count(_.iqType == IQT_MEM.litValue) == 1)
  require (issueParams.count(_.iqType == IQT_INT.litValue) == 1)

  val intIssueParam = issueParams.find(_.iqType == IQT_INT.litValue).get
  val memIssueParam = issueParams.find(_.iqType == IQT_MEM.litValue).get
  val fpIssueParam = issueParams.find(_.iqType == IQT_FP.litValue).get
  val combIssueParam = issueParams.find(_.iqType == IQT_COMB.litValue)

  val intWidth = intIssueParam.issueWidth
  val memWidth = memIssueParam.issueWidth


  if(!boomParams.unifiedIssueQueue){
    issueParams.map(x => require(x.dispatchWidth <= coreWidth && x.dispatchWidth > 0))
  }

  //************************************
  // Load/Store Unit
  val dcacheParams: DCacheParams = tileParams.dcache.get
  val icacheParams: ICacheParams = tileParams.icache.get
  val icBlockBytes = icacheParams.blockBytes

  require(icacheParams.nSets <= 64, "Handling aliases in the ICache is buggy.")

  val enableFastLoadUse = boomParams.enableFastLoadUse
  val enablePrefetching = boomParams.enablePrefetching
  val nLBEntries = dcacheParams.nMSHRs

  //************************************
  // Branch Prediction

  val enableBTB = boomParams.enableBTB
  val enableBTBContainsBranches = boomParams.enableBTBContainsBranches

  val enableBranchPredictor = boomParams.enableBranchPredictor

  val enableBpdUmodeOnly = boomParams.enableBpdUModeOnly
  val enableBpdUshistory = boomParams.enableBpdUSModeHistory
  // What is the maximum length of global history tracked?
  var globalHistoryLength = 0
  // What is the physical length of the VeryLongHistoryRegister? This must be
  // able to handle the GHIST_LENGTH as well as being able hold all speculative
  // updates well beyond the GHIST_LENGTH (i.e., +ROB_SZ and other buffering).
  var bpdInfoSize = 0

  val tageBpuParams = boomParams.tage
  val gshareBpuParams = boomParams.gshare
  val baseOnlyBpuParams = boomParams.bpdBaseOnly
  val randomBpuParams = boomParams.bpdRandom

  if (!enableBranchPredictor) {
    bpdInfoSize = 1
    globalHistoryLength = 1
  } else if (baseOnlyBpuParams.isDefined && baseOnlyBpuParams.get.enabled) {
    globalHistoryLength = 8
    bpdInfoSize = BaseOnlyBrPredictor.GetRespInfoSize()
  } else if (gshareBpuParams.isDefined && gshareBpuParams.get.enabled) {
    globalHistoryLength = gshareBpuParams.get.historyLength
    bpdInfoSize = GShareBrPredictor.GetRespInfoSize(globalHistoryLength)
  } else if (tageBpuParams.isDefined && tageBpuParams.get.enabled) {
    globalHistoryLength = tageBpuParams.get.historyLengths.max
    bpdInfoSize = TageBrPredictor.GetRespInfoSize(p)
  } else if (randomBpuParams.isDefined && randomBpuParams.get.enabled) {
    globalHistoryLength = 1
    bpdInfoSize = RandomBrPredictor.GetRespInfoSize()
  }

  //************************************
  // Extra Knobs and Features
  val enableCommitMapTable = boomParams.enableCommitMapTable
  require(!enableCommitMapTable) // TODO Fix the commit map table.
  val enableFastPNR = boomParams.enableFastPNR

  //************************************
  // Implicitly calculated constants
  val numRobRows      = numRobEntries/coreWidth
  val robAddrSz       = log2Ceil(numRobRows) + log2Ceil(coreWidth)
  // the f-registers are mapped into the space above the x-registers
  val logicalRegCount = if (usingFPU) 64 else 32
  val lregSz          = log2Ceil(logicalRegCount)
  val ipregSz         = log2Ceil(numIntPhysRegs)
  val fpregSz         = log2Ceil(numFpPhysRegs)
  val maxPregSz       = ipregSz max fpregSz
  val ldqAddrSz       = log2Ceil(numLdqEntries)
  val stqAddrSz       = log2Ceil(numStqEntries)
  val lsuAddrSz       = ldqAddrSz max stqAddrSz
  val brTagSz         = log2Ceil(maxBrCount)

  require (numIntPhysRegs >= (32 + coreWidth))
  require (numFpPhysRegs >= (32 + coreWidth))
  require (maxBrCount >=2)
  require (numRobEntries % coreWidth == 0)
  require ((numLdqEntries-1) > coreWidth)
  require ((numStqEntries-1) > coreWidth)


  //************************************
  // Other Non/Should-not-be sythesizable modules
  val useFetchMonitor = boomParams.useFetchMonitor

  //************************************
  // Non-BOOM parameters

  val corePAddrBits = paddrBits
  val corePgIdxBits = pgIdxBits
}


/**
 * Dromajo simulation parameters
 */
case class DromajoParams(
  bootromParams: Option[BootROMParams] = None,
  extMemParams: Option[MemoryPortParams] = None,
  clintParams: Option[CLINTParams] = None,
  plicParams: Option[PLICParams] = None
)

// CASINO Parameters

case class CasParams(
                    numInqEntries: Int = 8,
                    numSqEntries: Int = 8,
                    slidingOffset: Int = 1,
                    inqDispatches: Int = 2,
                    windowSize: Int = 2
                    )
{}

case class InoParams()

// Class for DnB Parameters
case class DnbParams(
                                numCrqEntries: Int = 8,
                                numDlqEntries: Int = 8,
                                dlqRobUrgentDist: Int = 2,
                                crqDispatches: Int = 1,
                                dlqDispatches: Int = 1
                              ){
  def dispatches(): Int = crqDispatches + dlqDispatches
}

// Case class for LoadSliceCore parameters.
//  TODO: Consider moving this to separate file?
// TODO: case class vs class
case class LoadSliceCoreParams(
                                numAqEntries: Int = 8,
                                numBqEntries: Int = 8,
                                unifiedIssueQueue: Boolean = false,
                                aDispatches: Int = 1,
                                bDispatches: Int = 1
){
  def dispatches(): Int = aDispatches+bDispatches

}

/**
  * IBDA Params, used by LsC and DnB
 */
case class IbdaParams(
                 ibdaTagType: Int = IBDA_TAG_FULL_PC,
                 rdtIstMarkWidth: Int = 4,
                 branchIbda: Boolean = false,
                 bloomIst: Boolean = false,
                 hashBits: Int = 0
                     )
{
  // TODO: ugly hack for now...
  val inBits = 59//(
//    UOPC_SZ+ //uopc 9
//    lregSz+ //ldst 6
//    lregSz+ //lrs1 6
//    lregSz+ //lrs2 6
//    lregSz+ //lrs3 6
//    log2Ceil(icBlockBytes)+ //pc_lob 6
//    LONGEST_IMM_SZ //imm_packed 20
//  )
  lazy val hash = Hash(inBits, hashBits)
  def ibda_get_tag(uop: MicroOp): UInt = {
    val tag = Wire(UInt(ibda_tag_sz.W))
    // IBDA_TAG_FULL_PC is handled in core
    if (ibdaTagType == IBDA_TAG_UOPC_LOB) tag := Cat(uop.uopc, uop.pc_lob)
    else if (ibdaTagType == IBDA_TAG_INST_LOB) tag := Cat(uop.inst, uop.pc_lob)
    else if (ibdaTagType == IBDA_TAG_DEBUG_PC) tag := uop.debug_pc
    else if (ibdaTagType == IBDA_TAG_DEBUG) tag := uop.debug_pc
    else if (ibdaTagType == IBDA_TAG_HASH) tag := hash(
      Cat(
        uop.uopc,
        uop.ldst,
        uop.lrs1,
        uop.lrs2,
        uop.lrs3,
        uop.pc_lob,
        uop.imm_packed,
      )
    )
    else if(ibdaTagType == IBDA_TAG_HASH_PC) tag := hash(uop.debug_pc)

    else require(false, "ibda_get_tag not implemented for this tag")
    tag
  }
  require((ibdaTagType != IBDA_TAG_HASH && ibdaTagType != IBDA_TAG_HASH_PC) || hashBits != 0)

  def rdtIstMarkSz: Int = {
    if (rdtIstMarkWidth == 1) {
      1
    } else {
      log2Ceil(rdtIstMarkWidth)
    }
  }

  // Get tag size.
  def ibda_tag_sz: Int = {
    ibdaTagType match {
      case IBDA_TAG_FULL_PC => 40
      case IBDA_TAG_DEBUG_PC => 40
      case IBDA_TAG_DEBUG => 40
      case IBDA_TAG_UOPC_LOB => UOPC_SZ + 6 //uopc + pc_lob
      case IBDA_TAG_INST_LOB => 32 + 6 //inst + pc_lob
      case IBDA_TAG_HASH => hashBits //inst + pc_lob
      case IBDA_TAG_HASH_PC => hashBits //inst + pc_lob
      case _ => {
        require(false, "ibda_tag_sz not implemented for this tag")
        0
      }
    }
  }

  def is_ibda(uop: MicroOp): Bool = {
    if(branchIbda){
      uop.is_lsc_b || uop.is_br_or_jmp
    } else{
      uop.is_lsc_b || uop.uopc === uopLD || uop.uopc === uopSTA || uop.uopc === uopSTD
    }
  }
}

/**
  * Busy lookup params. Deciding how we read the busy table of the REN stage
  */
case class BusyLookupParams(
                           lookupAtRename: Boolean = true, //Should we do busy-looup at ren2 (default behaviour of boom)
                           lookupAtDisWidth: Int = 0, // How many read ports should we have to the busy-table from the Dispatch port?
                           )
{
  // Unfortunate that we have to pass the coreWidth into this function. But we dont have that parameter accessible here.
  def busyTableReqWidth(plWidth: Int): Int = {
    if (lookupAtRename) {
      lookupAtDisWidth + plWidth
    } else {
      lookupAtDisWidth
    }
  }
}