package lsc

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}
import chisel3.util._

import scala.collection.immutable


class Hash_(inBits: Int, outBits: Int, random_data: Seq[UInt]) extends Module {
  val io = IO(new Bundle() {
    val in = Input(UInt(inBits.W))
    val out = Output(UInt(outBits.W))
  })
  io.out := Cat(random_data.map(d => (d(inBits - 1, 0) & io.in).asBools().reduce(_ ^ _)))
}

object Hash {
  val r = scala.util.Random
  //    probably would be a good idea to use a static seed??
  val seed = 1
  r.setSeed(seed)
  println(f"setting seed to ${seed}!!!!!!!!!!!!!!!")

  def callStack = try { throw new Exception} catch { case ex => ex.getStackTrace drop 2 }

  def printStackTrace = callStack drop 1 /* don't print ourselves! */ foreach println
  def apply(inBits: Int, outBits: Int): UInt => UInt = {
    require(inBits <= 63)
    val random_data: Seq[UInt] = (0 until outBits).map(_ => math.abs(r.nextLong()).U)
    // this is the identity hash - usefull for debugging
//    val random_data: Seq[UInt] = (0 until outBits).map(i => (1L << i).U).reverse
//    printStackTrace
    println("Hash Matrix:")
    random_data.foreach(u =>
      println(u.litValue().toLong.toBinaryString)
    )

    def fun(u: UInt): UInt = {
      val m = Module(new Hash_(inBits, outBits, random_data))
      m.suggestName("Hash")
      m.io.in := u
      m.io.out
    }

    fun
  }
}

// m is the number of bit in the bloom filter
// k is the number of hash functions
class BloomFilter(m: Int, k: Int, inBits: Int, reads: Int, collisionRate: Double = 0.01) extends Module {
  val bit_vector = RegInit(VecInit(Seq.fill(m)(false.B)))
  val log_m = log2Floor(m)
  val hash_functions = (0 until k).map(_ => Hash(inBits, log_m))
  val ctr = RegInit(0.U(log_m.W))

  val io = IO(new Bundle() {
    val insert = Input(Valid(UInt(inBits.W)))
    val test = Vec(reads, new Bundle() {
      val bits = Input(UInt(inBits.W))
      val found = Output(Bool())
    })
  })

  when(io.insert.valid) {
    for (h <- hash_functions) {
      val idx = h(io.insert.bits)
      bit_vector(idx) := true.B
      ctr := ctr + 1.U
    }
  }

  val max_cnt: Int = (math.log(1 - math.pow(collisionRate, 1.0 / k)) / (-k.toDouble / m)).toInt
  val full = ctr > max_cnt.U
  when(full) {
    bit_vector.map(_ := false.B)
  }

  for (t <- io.test) {
    var found = true.B
    for (h <- hash_functions) {
      val idx = h(t.bits)
      found = found && bit_vector(idx)
    }
    t.found := found
  }
}


// m is the number of bit in the bloom filter
// k is the number of hash functions
class BloomFilterModified(m: Int, k: Int, inBits: Int, reads: Int, collisionRate: Double = 0.01) extends Module {
  require(isPow2(m))
  val srams = (0 until k).map(_ => Module(new Sram(m, 1, reads)).io)
  val log_m = log2Floor(m)
  val hash_functions = (0 until k).map(_ => Hash(inBits, log_m))
  val ctr = RegInit(0.U(log_m.W))
  // flush after reset
  val flushing = RegInit(true.B)
  val flush_ctr = RegInit(0.U(log_m.W))

  val io = IO(new Bundle() {
    val insert = Input(Valid(UInt(inBits.W)))
    val test = Vec(reads, new Bundle() {
      val bits = Input(UInt(inBits.W))
      val found = Output(Bool())
    })
  })
  dontTouch(io)
  srams.foreach(s => {
    s.write.en := false.B
    s.write.data := DontCare
    s.write.addr := DontCare
  })
  // p = (1-math.e**((-n)/m))**k
  // src: https://www.wolframalpha.com/input/?i=solve+p+%3D+%281-e**%28%28-n%29%2Fm%29%29**k+for+n
  val max_cnt: Int = (-m * math.log(1 - math.pow(collisionRate, 1.0 / k))).toInt
  println(f"modified bloom - max_cnt: $max_cnt")
  val full = ctr > max_cnt.U
  when(full) {
    flushing := true.B
  }

  when(flushing) {
    srams.foreach(s => {
      s.write.addr := flush_ctr
      s.write.data := 0.U
      s.write.en := true.B
    })
    flush_ctr := flush_ctr + 1.U
    when(flush_ctr.andR()) {
      flushing := false.B
    }
  }.elsewhen(io.insert.valid) {
    for ((h, i) <- hash_functions.zipWithIndex) {
      val idx = WireInit(h(io.insert.bits))
      idx.suggestName(f"sram_write_hash_${i}")
      dontTouch(idx)
      srams(i).write.addr := idx
      srams(i).write.data := 1.U
      srams(i).write.en := true.B
      ctr := ctr + 1.U
    }
  }

  for ((t, j) <- io.test.zipWithIndex) {
    var found = true.B
    for ((h, i) <- hash_functions.zipWithIndex) {
      val idx = WireInit(h(t.bits))
      idx.suggestName(f"sram_read_${j}_hash_${i}")
      dontTouch(idx)
      srams(i).read(j).addr := idx
      found = found && srams(i).read(j).data.asBool()
    }
    t.found := found
  }
}

class HashTest(inBits: Int, outBits: Int) extends Module {
  val io = IO(new Bundle() {
    val in = Input(UInt(inBits.W))
    val out = Output(UInt(outBits.W))
  })
  val hash = Hash(inBits, outBits)
  io.out := hash(io.in)
}

class Sram(size: Int, width: Int, reads: Int, synchronous: Boolean = true) extends Module {
  val io = IO(new Bundle() {
    val write = Input(new Bundle {
      val addr = UInt(log2Floor(size).W)
      val data = UInt(width.W)
      val en = Bool()
    })
    val read = Vec(reads, new Bundle() {
      val addr = Input(UInt(log2Floor(size).W))
      val data = Output(UInt(width.W))
    })
  })
  val sram: MemBase[UInt] = if(synchronous) SyncReadMem(size, UInt(width.W)) else Mem(size, UInt(width.W))
  when(io.write.en) {
    sram.write(io.write.addr, io.write.data)
  }
  io.read.foreach(r => r.data := sram.read(r.addr))
}

object GenerateVerilog extends App {
  chisel3.Driver.execute(args, () => new BloomFilterModified(m = 2048, k = 2, inBits = 32, reads = 2, collisionRate = 0.01))
  //  chisel3.Driver.execute(args, () => new HashTest(32, 12))
  //  chisel3.Driver.execute(args, () => new DistRamTest(2048, 1, 2))
}

object BloomTester extends App {
    chisel3.iotesters.Driver(() => new BloomFilterModified(m = 2048, k = 6, inBits = 32, reads = 1, collisionRate = 0.0001), "verilator") {
      (c) =>
        new PeekPokeTester(c) {
          // init everything to false
          poke(c.io.insert.valid, false.B)
          poke(c.io.insert.bits, 0.U)
          step(1)
          val r = scala.util.Random
          val added_values = (0 until c.max_cnt).map(_ => r.nextInt())
          for (i <- added_values){
            poke(c.io.test(0).bits, i)
            step(1)
            if(peek(c.io.test(0).found)!= 0){
              println(f"$i already found")
            }
            poke(c.io.insert.valid, true.B)
            poke(c.io.insert.bits, i)
            step(1)
            poke(c.io.insert.valid, false.B)
            poke(c.io.test(0).bits, i)
            step(1)
            expect(c.io.test(0).found, 1)
          }
          println(f"added ${c.max_cnt} values")
          println(f"testing if they are found")
          for (i <- added_values) {
            poke(c.io.test(0).bits, i)
            step(1)
            expect(c.io.test(0).found, 1)
          }
          println(f"testing random numbers")
          val iterations = 10000000
          var found_cnt = 0
          for(i <- (0 until iterations)){
            val ri = r.nextInt()
            poke(c.io.test(0).bits, ri)
            step(1)
            if(peek(c.io.test(0).found)!= 0){
//              println(f"$ri already found")
              found_cnt+=1
            }
          }
          println(f"found: $found_cnt iterations: $iterations false positive rate: ${found_cnt.toDouble/iterations}")
          println("adding one more to flush")
          poke(c.io.insert.valid, true.B)
          poke(c.io.insert.bits, 0)
          step(1)
          poke(c.io.insert.valid, false.B)
          step(10000)
          println(f"testing if flush worked")
          for (i <- added_values) {
            poke(c.io.test(0).bits, i)
            step(1)
            expect(c.io.test(0).found, 0)
          }
        }
    }
}

object HashTester extends App {
  val outBits = 8
  // how many bits of entropy to use for the reduced case
  val bitsReduced = 10
  val numTests = 1 << 18
  val numSlots = 1 << outBits
  val inBits = 32
  val r = scala.util.Random
  for (j <- 0 until 100) {
    chisel3.iotesters.Driver(() => new HashTest(inBits, outBits), "treadle") {
      (c) =>
        new PeekPokeTester(c) {
          def chiSqTest(a: Array[Int]): Double ={
            val expected = numTests/numSlots.toDouble
            a.map(n => math.pow(n-expected, 2)/expected).sum
          }
          def uniformityTest(a: Array[Int]): Double ={
            a.map(n => n*(n+1)/2.0).sum/(
              (numTests/(2.0*numSlots))*(numTests+(2.0*numSlots)-1)
              )
          }
          val countArray = Seq.fill(numSlots)(0).toArray
          val countReducedArray = Seq.fill(numSlots)(0).toArray
          val referenceArray = Seq.fill(numSlots)(0).toArray
          for (i <- 0 until numTests) {
            val ri = r.nextInt()//r.nextLong() & ((1L<<inBits)-1)
            poke(c.io.in, ri)
            val tmp = peek(c.io.out).bigInteger.intValue()
//            println(f"$tmp $ri")
            countArray(tmp) += 1
            // use hash with only the bitsReduced last bits contributing entropy
            poke(c.io.in, ri&((1<<bitsReduced)-1))
            val tmp2 = peek(c.io.out).bigInteger.intValue()
            countReducedArray(tmp2) += 1
            // outBits last bits of random number
            referenceArray(ri & (numSlots-1)) += 1
          }
          println(f"count - min ${countArray.min}, max ${countArray.max}, chi_sq ${chiSqTest(countArray)}, unif ${uniformityTest(countArray)}")
          println(f"count_reduced - min ${countReducedArray.min}, max ${countReducedArray.max}, chi_sq ${chiSqTest(countReducedArray)}, unif ${uniformityTest(countReducedArray)}")
          println(f"ref - min ${referenceArray.min}, max ${referenceArray.max}, chi_sq ${chiSqTest(referenceArray)}, unif ${uniformityTest(referenceArray)}")
        }
    }
  }
}