////******************************************************************************
//// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
//// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
////------------------------------------------------------------------------------
//
//package boom.tests.lsc
//
//import boom.common.{BoomTilesKey, LoadSliceCoreParams}
//import boom.exu.{RegisterFile, RegisterFileSynthesizable, SliceDispatchQueue}
//import boom.tests.BoomTestUtils
//import boom.tests.BoomTestUtils.{augment, lookupByHartId}
//import chipsalliance.rocketchip.config.{Config, Parameters}
//import chisel3._
//import chisel3.iotesters._
//import freechips.rocketchip.config.Parameters
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.tile.{LookupByHartId, TileKey}
//
//import scala.util.Random
//
///**
// * Main register file tester
// */
//
//class SliceBoomTestConfig extends Config(
//    new boom.common.WithSliceBooms ++                         // 1-wide BOOM
//    new boom.common.WithNBoomCores(1) ++                      // single-core
//    new freechips.rocketchip.system.BaseConfig)
//
//class SliceQueueTester extends ChiselFlatSpec
//{
//  // boom parameters (and modifications to them)
//  val origParams: Parameters = new SliceBoomTestConfig ++ Parameters.empty
//  val boomTileParams = origParams(BoomTilesKey) // this is a seq
//
//  // augment the parameters
//  val boomParams = augment(boomTileParams(0))(origParams)
//
//
//  //BoomTestUtils.getBoomParameters("SliceBoomConfig", configPackage = "example")
//
//
//  // test both regfiles and iterations of the r/w port counts
//  for (enq_width <- 1 to 1)
//  {
//    for (deq_width <- 1 to enq_width)
//    {
//      implicit val p: Parameters = boomParams.alterPartial
//      {
//        case BoomTilesKey => boomParams(BoomTilesKey) map { r =>
//          r.copy(
//            core = r.core.copy(
//              decodeWidth = enq_width,
//              loadSliceCore = Some(LoadSliceCoreParams(
//
//              )),
//            )
//          )
//        }
//      }
//
//      def queue = new SliceDispatchQueue(qName = "test_q", deqWidth = deq_width)
//
//      // note: the "it" keyword copies the "behavior"
//      behavior of s"""SliceDispatchQueue with: enq=$enq_width, deq=$deq_width"""
//
//      // ----------------
//      // TESTING SECTION
//      // ----------------
//
//      it should s"work" in
//      {
//        chisel3.iotesters.Driver(() => queue, "treadle")
//        {
//          (c) => new ZeroRegisterTest(c, enq_width, deq_width)
//        } should be (true)
//      }
//    }
//  }
//}
//
///**
// * Read/writes from register preg0 and make sure that it gets 0
// */
//class ZeroRegisterTest[R <: SliceDispatchQueue](
//  c: R,
//  enq_width: Int,
//  deq_width: Int) extends PeekPokeTester(c)
//{
//  // enq not valid
//  for(i <- 0 until enq_width){
//    poke(c.io.enq_uops(i).valid, false.B)
//  }
//  // deq not ready
//  for(i <- 0 until deq_width){
//    poke(c.io.heads(i).ready, false.B)
//  }
//  // no branch or flush
//  poke(c.io.brinfo.valid, false.B)
//  poke(c.io.flush, false.B)
//  // static tsc val for now
//  poke(c.io.tsc_reg, 1337.U)
//  for(i <- 0 until 10){
//    step(1)
//  }
//}