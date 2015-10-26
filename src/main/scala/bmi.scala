package junctions

import Chisel._

/**
 * Chisel junctions seems to be missing a generic memory interface for
 * peripherals with byte oriented operations support, so I decided to
 * add one based on SMI. It is almost identical but one feature: dataWidth
 * is now always fraction of 8bits (a Byte) and be (Byte Enable) signal add.
 */
class BMIReq(val dataBytes: Int, val addrWidth: Int) extends Bundle {
  val rw = Bool()
  val addr = UInt(width = addrWidth)
  val data = Bits(width = dataBytes * 8)
  val be = Bits(width = dataBytes)

  override def cloneType =
    new BMIReq(dataBytes, addrWidth).asInstanceOf[this.type]
}

/** Bytes Memory Interface IO. Used as an abstraction level for peripherals
 *  @param dataBytes the width in Bytes (8 bits) of the data field
 *  @param addrWidth the width in bits of the addr field */
class BMIIO(val dataBytes: Int, val addrWidth: Int) extends Bundle {
  val req = Decoupled(new BMIReq(dataBytes, addrWidth))
  val resp = Decoupled(Bits(width = dataBytes * 8)).flip

  override def cloneType =
    new BMIIO(dataBytes, addrWidth).asInstanceOf[this.type]
}

abstract class BMIPeripheral extends Module {
  val dataBytes: Int
  val addrWidth: Int

  lazy val io = new BMIIO(dataBytes, addrWidth).flip
}

/** A simple random access memory through BMI */
class BMIMem(val dataBytes: Int, val memDepth: Int) extends BMIPeripheral {
  // override
  val addrWidth = log2Up(memDepth)

  val mem = Mem(memDepth, Bits(width = dataBytes * 8))
  
  var wmask = Fill(8, io.req.bits.be(0))
  for (i <- 1 until dataBytes - 1) {
    wmask = Fill(8, io.req.bits.be(i)) ## wmask
  }

  val ren = io.req.fire() && !io.req.bits.rw
  val wen = io.req.fire() && io.req.bits.rw

  when (wen) { mem.write(io.req.bits.addr, io.req.bits.data, wmask) }

  io.resp.valid := ren
  io.resp.bits := mem.read(io.req.bits.addr)
  io.req.ready := Bool(true)
}
