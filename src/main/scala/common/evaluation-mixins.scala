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