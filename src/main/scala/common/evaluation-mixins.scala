package boom.common

import chisel3._
import chisel3.util.log2Up
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.subsystem.{RocketCrossingParams, RocketTilesKey, SystemBusKey}
import freechips.rocketchip.devices.tilelink.BootROMParams
import freechips.rocketchip.diplomacy.{AsynchronousCrossing, RationalCrossing, SynchronousCrossing}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import boom.ifu._
import boom.exu.{IssueParams, _}
import boom.lsu._


// Mixin to add Performance Counters to the Micro-Ops and update at commit
class WithQueuePerfCounters() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {

    require(b.core.loadSliceMode || b.core.dnbMode || b.core.casMode, "Queue Perf Counters only for CAS/DNB/LSC")

    b.core.copy(
      queuePerfCounters = true
    )
  })}
})



class WithOriginalIbda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_FULL_PC,
        rdtIstMarkWidth = 4,
      ))
    )
  })}
})

class WithOriginalDebugIbda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_DEBUG_PC,
        rdtIstMarkWidth = 4,
      ))
    )
  })}
})

class WithOneBitPcIbda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_FULL_PC,
        rdtIstMarkWidth = 1,
      ))
    )
  })}
})

class WithOneBitDebugPcIbda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_DEBUG_PC,
        rdtIstMarkWidth = 1,
      ))
    )
  })}
})

class WithDebugIbda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_DEBUG,
        rdtIstMarkWidth = 1,
      ))
    )
  })}
})

class WithHash14Ibda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_HASH,
        rdtIstMarkWidth = 1,
        hashBits = 14,
      ))
    )
  })}
})

class WithHashPc14Ibda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_HASH_PC,
        rdtIstMarkWidth = 1,
        hashBits = 14,
      ))
    )
  })}
})

class WithHashPc40Ibda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_HASH_PC,
        rdtIstMarkWidth = 1,
        hashBits = 40,
      ))
    )
  })}
})

class WithBloomIbda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_INST_LOB,
        rdtIstMarkWidth = 1,
        bloomIst = true
      ))
    )
  })}
})

class WithBloomPcIbda() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.loadSliceMode || b.core.dnbMode, "IBDA only for LSC and DNB")
    b.core.copy(
      ibdaParams = Some(IbdaParams(
        ibdaTagType = IBDA_TAG_DEBUG_PC,
        rdtIstMarkWidth = 1,
        bloomIst = true
      ))
    )
  })}
})

class WithNaiveQueue16() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.inoParams.exists(_.queueMode), "need queue mode!")
    b.core.copy(
      inoParams = b.core.inoParams.map(i => i.copy(
        queueParams = DispatchQueueParams(
          numEntries = 8,
          qName="INQ",
          deqWidth=2,
          enqWidth=2,
          stallOnUse = true,
          headRegisters = false
        ),
        queueTypes = QUEUE_NAIVE
      ))
    )
  })}
})

class WithSingleQueue16() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.inoParams.exists(_.queueMode), "need queue mode!")
    b.core.copy(
      inoParams = b.core.inoParams.map(i => i.copy(
        queueParams = DispatchQueueParams(
          numEntries = 8,
          qName="INQ",
          deqWidth=2,
          enqWidth=2,
          stallOnUse = true,
          headRegisters = false
        ),
        queueTypes = QUEUE_SINGLE
      ))
    )
  })}
})

class WithSingleQueue16Head() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.inoParams.exists(_.queueMode), "need queue mode!")
    b.core.copy(
      inoParams = b.core.inoParams.map(i => i.copy(
        queueParams = DispatchQueueParams(
          numEntries = 8,
          qName="INQ",
          deqWidth=2,
          enqWidth=2,
          stallOnUse = false,
          headRegisters = true
        ),
        queueTypes = QUEUE_SINGLE
      ))
    )
  })}
})

class WithMultiQueue16() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.inoParams.exists(_.queueMode), "need queue mode!")
    b.core.copy(
      inoParams = b.core.inoParams.map(i => i.copy(
        queueParams = DispatchQueueParams(
          numEntries = 8,
          qName="INQ",
          deqWidth=2,
          enqWidth=2,
          stallOnUse = true,
          headRegisters = false
        ),
        queueTypes = QUEUE_MULTI
      ))
    )
  })}
})

class WithMultiQueue16Head() extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = {
    require(b.core.inoParams.exists(_.queueMode), "need queue mode!")
    b.core.copy(
      inoParams = b.core.inoParams.map(i => i.copy(
        queueParams = DispatchQueueParams(
          numEntries = 8,
          qName="INQ",
          deqWidth=2,
          enqWidth=2,
          stallOnUse = false,
          headRegisters = true
        ),
        queueTypes = QUEUE_MULTI
      ))
    )
  })}
})