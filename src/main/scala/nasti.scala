/// See LICENSE for license details.

package junctions
import Chisel._
import scala.math.max
import scala.collection.mutable.ArraySeq

case object NastiKey extends Field[NastiParameters]

case class NastiParameters(dataBits: Int, addrBits: Int, idBits: Int)

trait HasNastiParameters {
  implicit val p: Parameters
  val external = p(NastiKey)
  val nastiXDataBits = external.dataBits
  val nastiWStrobeBits = nastiXDataBits / 8
  val nastiXAddrBits = external.addrBits
  val nastiWIdBits = external.idBits
  val nastiRIdBits = external.idBits
  val nastiXIdBits = max(nastiWIdBits, nastiRIdBits)
  val nastiXUserBits = 1
  val nastiAWUserBits = nastiXUserBits
  val nastiWUserBits = nastiXUserBits
  val nastiBUserBits = nastiXUserBits
  val nastiARUserBits = nastiXUserBits
  val nastiRUserBits = nastiXUserBits
  val nastiXLenBits = 8
  val nastiXSizeBits = 3
  val nastiXBurstBits = 2
  val nastiXCacheBits = 4
  val nastiXProtBits = 3
  val nastiXQosBits = 4
  val nastiXRegionBits = 4
  val nastiXRespBits = 2

  def bytesToXSize(bytes: UInt) = MuxLookup(bytes, UInt("b111"), Array(
    UInt(1) -> UInt(0),
    UInt(2) -> UInt(1),
    UInt(4) -> UInt(2),
    UInt(8) -> UInt(3),
    UInt(16) -> UInt(4),
    UInt(32) -> UInt(5),
    UInt(64) -> UInt(6),
    UInt(128) -> UInt(7)))
}

abstract class NastiModule(implicit val p: Parameters) extends Module
  with HasNastiParameters
abstract class NastiBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasNastiParameters

abstract class NastiChannel(implicit p: Parameters) extends NastiBundle()(p)
abstract class NastiMasterToSlaveChannel(implicit p: Parameters) extends NastiChannel()(p)
abstract class NastiSlaveToMasterChannel(implicit p: Parameters) extends NastiChannel()(p)

trait HasNastiMetadata extends HasNastiParameters {
  val addr   = UInt(width = nastiXAddrBits)
  val len    = UInt(width = nastiXLenBits)
  val size   = UInt(width = nastiXSizeBits)
  val burst  = UInt(width = nastiXBurstBits)
  val lock   = Bool()
  val cache  = UInt(width = nastiXCacheBits)
  val prot   = UInt(width = nastiXProtBits)
  val qos    = UInt(width = nastiXQosBits)
  val region = UInt(width = nastiXRegionBits)
}

trait HasNastiData extends HasNastiParameters {
  val data = UInt(width = nastiXDataBits)
  val last = Bool()
}

class NastiIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val aw = Decoupled(new NastiWriteAddressChannel)
  val w  = Decoupled(new NastiWriteDataChannel)
  val b  = Decoupled(new NastiWriteResponseChannel).flip
  val ar = Decoupled(new NastiReadAddressChannel)
  val r  = Decoupled(new NastiReadDataChannel).flip
}

class NastiAddressChannel(implicit p: Parameters) extends NastiMasterToSlaveChannel()(p)
    with HasNastiMetadata

class NastiResponseChannel(implicit p: Parameters) extends NastiSlaveToMasterChannel()(p) {
  val resp = UInt(width = nastiXRespBits)
}

class NastiWriteAddressChannel(implicit p: Parameters) extends NastiAddressChannel()(p) {
  val id   = UInt(width = nastiWIdBits)
  val user = UInt(width = nastiAWUserBits)
}

class NastiWriteDataChannel(implicit p: Parameters) extends NastiMasterToSlaveChannel()(p)
    with HasNastiData {
  val strb = UInt(width = nastiWStrobeBits)
  val user = UInt(width = nastiWUserBits)
}

class NastiWriteResponseChannel(implicit p: Parameters) extends NastiResponseChannel()(p) {
  val id   = UInt(width = nastiWIdBits)
  val user = UInt(width = nastiBUserBits)
}

class NastiReadAddressChannel(implicit p: Parameters) extends NastiAddressChannel()(p) {
  val id   = UInt(width = nastiRIdBits)
  val user = UInt(width = nastiARUserBits)
}

class NastiReadDataChannel(implicit p: Parameters) extends NastiResponseChannel()(p)
    with HasNastiData {
  val id   = UInt(width = nastiRIdBits)
  val user = UInt(width = nastiRUserBits)
}

object NastiWriteAddressChannel {
  def apply(id: UInt, addr: UInt, size: UInt, len: UInt = UInt(0))(implicit p: Parameters) = {
    val aw = Wire(new NastiWriteAddressChannel)
    aw.id := id
    aw.addr := addr
    aw.len := len
    aw.size := size
    aw.burst := UInt("b01")
    aw.lock := Bool(false)
    aw.cache := UInt("b0000")
    aw.prot := UInt("b000")
    aw.qos := UInt("b0000")
    aw.region := UInt("b0000")
    aw.user := UInt(0)
    aw
  }
}

object NastiReadAddressChannel {
  def apply(id: UInt, addr: UInt, size: UInt, len: UInt = UInt(0))(implicit p: Parameters) = {
    val ar = Wire(new NastiReadAddressChannel)
    ar.id := id
    ar.addr := addr
    ar.len := len
    ar.size := size
    ar.burst := UInt("b01")
    ar.lock := Bool(false)
    ar.cache := UInt(0)
    ar.prot := UInt(0)
    ar.qos := UInt(0)
    ar.region := UInt(0)
    ar.user := UInt(0)
    ar
  }
}

object NastiWriteDataChannel {
  def apply(data: UInt, last: Bool = Bool(true))(implicit p: Parameters): NastiWriteDataChannel = {
    val w = Wire(new NastiWriteDataChannel)
    w.strb := Fill(w.nastiWStrobeBits, UInt(1, 1))
    w.data := data
    w.last := last
    w.user := UInt(0)
    w
  }
  def apply(data: UInt, strb: UInt, last: Bool)
           (implicit p: Parameters): NastiWriteDataChannel = {
    val w = apply(data, last)
    w.strb := strb
    w
  }
}

object NastiReadDataChannel {
  def apply(id: UInt, data: UInt, last: Bool = Bool(true), resp: UInt = UInt(0))(
      implicit p: Parameters) = {
    val r = Wire(new NastiReadDataChannel)
    r.id := id
    r.data := data
    r.last := last
    r.resp := resp
    r.user := UInt(0)
    r
  }
}

object NastiWriteResponseChannel {
  def apply(id: UInt, resp: UInt = UInt(0))(implicit p: Parameters) = {
    val b = Wire(new NastiWriteResponseChannel)
    b.id := id
    b.resp := resp
    b.user := UInt(0)
    b
  }
}

class MemIONastiIOConverter(cacheBlockOffsetBits: Int)(implicit p: Parameters) extends MIFModule
    with HasNastiParameters {
  val io = new Bundle {
    val nasti = (new NastiIO).flip
    val mem = new MemIO
  }

  require(mifDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree")
  val (mif_cnt_out, mif_wrap_out) = Counter(io.mem.resp.fire(), mifDataBeats)

  assert(!io.nasti.aw.valid || io.nasti.aw.bits.size === UInt(log2Up(mifDataBits/8)),
    "Nasti data size does not match MemIO data size")
  assert(!io.nasti.ar.valid || io.nasti.ar.bits.size === UInt(log2Up(mifDataBits/8)),
    "Nasti data size does not match MemIO data size")
  assert(!io.nasti.aw.valid || io.nasti.aw.bits.len === UInt(mifDataBeats - 1),
    "Nasti length does not match number of MemIO beats")
  assert(!io.nasti.ar.valid || io.nasti.ar.bits.len === UInt(mifDataBeats - 1),
    "Nasti length does not match number of MemIO beats")

  // according to the spec, we can't send b until the last transfer on w
  val b_ok = Reg(init = Bool(true))
  when (io.nasti.aw.fire()) { b_ok := Bool(false) }
  when (io.nasti.w.fire() && io.nasti.w.bits.last) { b_ok := Bool(true) }

  val id_q = Module(new Queue(UInt(width = nastiWIdBits), 2))
  id_q.io.enq.valid := io.nasti.aw.valid
  id_q.io.enq.bits := io.nasti.aw.bits.id
  id_q.io.deq.ready := io.nasti.b.ready && b_ok

  io.mem.req_cmd.bits.addr := Mux(io.nasti.aw.valid, io.nasti.aw.bits.addr, io.nasti.ar.bits.addr) >>
                                UInt(cacheBlockOffsetBits)
  io.mem.req_cmd.bits.tag := Mux(io.nasti.aw.valid, io.nasti.aw.bits.id, io.nasti.ar.bits.id)
  io.mem.req_cmd.bits.rw := io.nasti.aw.valid
  io.mem.req_cmd.valid := (io.nasti.aw.valid && id_q.io.enq.ready) || io.nasti.ar.valid
  io.nasti.ar.ready := io.mem.req_cmd.ready && !io.nasti.aw.valid
  io.nasti.aw.ready := io.mem.req_cmd.ready && id_q.io.enq.ready

  io.nasti.b.valid := id_q.io.deq.valid && b_ok
  io.nasti.b.bits.id := id_q.io.deq.bits
  io.nasti.b.bits.resp := UInt(0)

  io.nasti.w.ready := io.mem.req_data.ready
  io.mem.req_data.valid := io.nasti.w.valid
  io.mem.req_data.bits.data := io.nasti.w.bits.data
  assert(!io.nasti.w.valid || io.nasti.w.bits.strb.andR, "MemIO must write full cache line")

  io.nasti.r.valid := io.mem.resp.valid
  io.nasti.r.bits.data := io.mem.resp.bits.data
  io.nasti.r.bits.last := mif_wrap_out
  io.nasti.r.bits.id := io.mem.resp.bits.tag
  io.nasti.r.bits.resp := UInt(0)
  io.mem.resp.ready := io.nasti.r.ready
}

/** Arbitrate among arbN masters requesting to a single slave */
class NastiArbiter(val arbN: Int)(implicit p: Parameters) extends NastiModule {
  val io = new Bundle {
    val master = Vec(new NastiIO, arbN).flip
    val slave = new NastiIO
  }

  if (arbN > 1) {
    val arbIdBits = log2Up(arbN)

    val ar_arb = Module(new RRArbiter(new NastiReadAddressChannel, arbN))
    val aw_arb = Module(new RRArbiter(new NastiWriteAddressChannel, arbN))

    val slave_r_arb_id = io.slave.r.bits.id(arbIdBits - 1, 0)
    val slave_b_arb_id = io.slave.b.bits.id(arbIdBits - 1, 0)

    val w_chosen = Reg(UInt(width = arbIdBits))
    val w_done = Reg(init = Bool(true))

    when (aw_arb.io.out.fire()) {
      w_chosen := aw_arb.io.chosen
      w_done := Bool(false)
    }

    when (io.slave.w.fire() && io.slave.w.bits.last) {
      w_done := Bool(true)
    }

    for (i <- 0 until arbN) {
      val m_ar = io.master(i).ar
      val m_aw = io.master(i).aw
      val m_r = io.master(i).r
      val m_b = io.master(i).b
      val a_ar = ar_arb.io.in(i)
      val a_aw = aw_arb.io.in(i)
      val m_w = io.master(i).w

      a_ar <> m_ar
      a_ar.bits.id := Cat(m_ar.bits.id, UInt(i, arbIdBits))

      a_aw <> m_aw
      a_aw.bits.id := Cat(m_aw.bits.id, UInt(i, arbIdBits))

      m_r.valid := io.slave.r.valid && slave_r_arb_id === UInt(i)
      m_r.bits := io.slave.r.bits
      m_r.bits.id := io.slave.r.bits.id >> UInt(arbIdBits)

      m_b.valid := io.slave.b.valid && slave_b_arb_id === UInt(i)
      m_b.bits := io.slave.b.bits
      m_b.bits.id := io.slave.b.bits.id >> UInt(arbIdBits)

      m_w.ready := io.slave.w.ready && w_chosen === UInt(i) && !w_done
    }

    io.slave.r.ready := io.master(slave_r_arb_id).r.ready
    io.slave.b.ready := io.master(slave_b_arb_id).b.ready

    io.slave.w.bits := io.master(w_chosen).w.bits
    io.slave.w.valid := io.master(w_chosen).w.valid && !w_done

    io.slave.ar <> ar_arb.io.out

    io.slave.aw.bits <> aw_arb.io.out.bits
    io.slave.aw.valid := aw_arb.io.out.valid && w_done
    aw_arb.io.out.ready := io.slave.aw.ready && w_done

  } else { io.slave <> io.master.head }
}

/** Locking RR arbiter for Nasti read data channel
 *  Arbiter locks until last message in channel is sent */
class NastiReadDataArbiter(arbN: Int)(implicit p: Parameters) extends NastiModule {
  val io = new Bundle {
    val in = Vec(Decoupled(new NastiReadDataChannel), arbN).flip
    val out = Decoupled(new NastiReadDataChannel)
  }

  def rotateLeft[T <: Data](norm: Vec[T], rot: UInt): Vec[T] = {
    val n = norm.size
    Vec.tabulate(n) { i =>
      Mux(rot < UInt(n - i), norm(UInt(i) + rot), norm(rot - UInt(n - i)))
    }
  }

  val lockIdx = Reg(init = UInt(0, log2Up(arbN)))
  val locked = Reg(init = Bool(false))

  // use rotation to give priority to the input after the last one granted
  val choice = PriorityMux(
    rotateLeft(Vec(io.in.map(_.valid)), lockIdx + UInt(1)),
    rotateLeft(Vec((0 until arbN).map(UInt(_))), lockIdx + UInt(1)))

  val chosen = Mux(locked, lockIdx, choice)

  for (i <- 0 until arbN) {
    io.in(i).ready := io.out.ready && chosen === UInt(i)
  }

  io.out.valid := io.in(chosen).valid
  io.out.bits := io.in(chosen).bits

  when (io.out.fire()) {
    when (!locked) {
      lockIdx := choice
      locked := !io.out.bits.last
    } .elsewhen (io.out.bits.last) {
      locked := Bool(false)
    }
  }
}

/** A slave that send decode error for every request it receives */
class NastiErrorSlave(implicit p: Parameters) extends NastiModule {
  val io = (new NastiIO).flip

  when (io.ar.fire()) { printf("Invalid read address %x\n", io.ar.bits.addr) }
  when (io.aw.fire()) { printf("Invalid write address %x\n", io.aw.bits.addr) }

  val r_queue = Module(new Queue(new NastiReadAddressChannel, 2))
  r_queue.io.enq <> io.ar

  val responding = Reg(init = Bool(false))
  val beats_left = Reg(init = UInt(0, nastiXLenBits))

  when (!responding && r_queue.io.deq.valid) {
    responding := Bool(true)
    beats_left := r_queue.io.deq.bits.len
  }

  io.r.valid := r_queue.io.deq.valid && responding
  io.r.bits.id := r_queue.io.deq.bits.id
  io.r.bits.data := UInt(0)
  io.r.bits.resp := Bits("b11")
  io.r.bits.last := beats_left === UInt(0)

  r_queue.io.deq.ready := io.r.fire() && io.r.bits.last

  when (io.r.fire()) {
    when (beats_left === UInt(0)) {
      responding := Bool(false)
    } .otherwise {
      beats_left := beats_left - UInt(0)
    }
  }

  val draining = Reg(init = Bool(false))
  io.w.ready := draining

  when (io.aw.fire()) { draining := Bool(true) }
  when (io.w.fire() && io.w.bits.last) { draining := Bool(false) }

  val b_queue = Module(new Queue(UInt(width = nastiWIdBits), 2))
  b_queue.io.enq.valid := io.aw.valid && !draining
  b_queue.io.enq.bits := io.aw.bits.id
  io.aw.ready := b_queue.io.enq.ready && !draining
  io.b.valid := b_queue.io.deq.valid && !draining
  io.b.bits.id := b_queue.io.deq.bits
  io.b.bits.resp := Bits("b11")
  b_queue.io.deq.ready := io.b.ready && !draining
}

/** Take a single Nasti master and route its requests to various slaves
 *  @param addrmap a sequence of base address + memory size pairs,
 *  on for each slave interface */
class NastiRouter(addrmap: Seq[(BigInt, BigInt)])(implicit p: Parameters) extends NastiModule {
  val nSlaves = addrmap.size

  val io = new Bundle {
    val master = (new NastiIO).flip
    val slave = Vec(new NastiIO, nSlaves)
  }

  var ar_ready = Bool(false)
  var aw_ready = Bool(false)
  var w_ready = Bool(false)
  var r_valid_addr = Bool(false)
  var w_valid_addr = Bool(false)

  addrmap.zip(io.slave).zipWithIndex.foreach { case (((base, size), s), i) =>
    val bound = base + size

    require(bigIntPow2(size),
      s"Region size $size is not a power of 2")
    require(base % size == 0,
      f"Region base address $base%x not divisible by $size%d" )

    val ar_addr = io.master.ar.bits.addr
    val ar_match = ar_addr >= UInt(base) && ar_addr < UInt(bound)

    s.ar.valid := io.master.ar.valid && ar_match
    s.ar.bits := io.master.ar.bits
    ar_ready = ar_ready || (s.ar.ready && ar_match)
    r_valid_addr = r_valid_addr || ar_match

    val aw_addr = io.master.aw.bits.addr
    val aw_match = aw_addr >= UInt(base) && aw_addr < UInt(bound)

    s.aw.valid := io.master.aw.valid && aw_match
    s.aw.bits := io.master.aw.bits
    aw_ready = aw_ready || (s.aw.ready && aw_match)
    w_valid_addr = w_valid_addr || aw_match

    val chosen = Reg(init = Bool(false))
    when (s.aw.fire()) { chosen := Bool(true) }
    when (s.w.fire() && s.w.bits.last) { chosen := Bool(false) }

    s.w.valid := io.master.w.valid && chosen
    s.w.bits := io.master.w.bits
    w_ready = w_ready || (s.w.ready && chosen)
  }

  val err_slave = Module(new NastiErrorSlave)
  err_slave.io.ar.valid := !r_valid_addr && io.master.ar.valid
  err_slave.io.ar.bits := io.master.ar.bits
  err_slave.io.aw.valid := !w_valid_addr && io.master.aw.valid
  err_slave.io.aw.bits := io.master.aw.bits
  err_slave.io.w.valid := io.master.w.valid
  err_slave.io.w.bits := io.master.w.bits

  io.master.ar.ready := ar_ready || (!r_valid_addr && err_slave.io.ar.ready)
  io.master.aw.ready := aw_ready || (!w_valid_addr && err_slave.io.aw.ready)
  io.master.w.ready := w_ready || err_slave.io.w.ready

  val b_arb = Module(new RRArbiter(new NastiWriteResponseChannel, nSlaves + 1))
  val r_arb = Module(new NastiReadDataArbiter(nSlaves + 1))

  for (i <- 0 until nSlaves) {
    b_arb.io.in(i) <> io.slave(i).b
    r_arb.io.in(i) <> io.slave(i).r
  }

  b_arb.io.in(nSlaves) <> err_slave.io.b
  r_arb.io.in(nSlaves) <> err_slave.io.r

  io.master.b <> b_arb.io.out
  io.master.r <> r_arb.io.out
}

/** Crossbar between multiple Nasti masters and slaves
 *  @param nMasters the number of Nasti masters
 *  @param nSlaves the number of Nasti slaves
 *  @param addrmap a sequence of base - size pairs;
 *  size of addrmap should be nSlaves */
class NastiCrossbar(nMasters: Int, nSlaves: Int, addrmap: Seq[(BigInt, BigInt)])
                   (implicit p: Parameters) extends NastiModule {
  val io = new Bundle {
    val masters = Vec(new NastiIO, nMasters).flip
    val slaves = Vec(new NastiIO, nSlaves)
  }

  val routers = Vec.fill(nMasters) { Module(new NastiRouter(addrmap)).io }
  val arbiters = Vec.fill(nSlaves) { Module(new NastiArbiter(nMasters)).io }

  for (i <- 0 until nMasters) {
    routers(i).master <> io.masters(i)
  }

  for (i <- 0 until nSlaves) {
    arbiters(i).master <> Vec(routers.map(r => r.slave(i)))
    io.slaves(i) <> arbiters(i).slave
  }
}

class NastiInterconnectIO(val nMasters: Int, val nSlaves: Int)
                         (implicit p: Parameters) extends Bundle {
  /* This is a bit confusing. The interconnect is a slave to the masters and
   * a master to the slaves. Hence why the declarations seem to be backwards. */
  val masters = Vec(new NastiIO, nMasters).flip
  val slaves = Vec(new NastiIO, nSlaves)
  override def cloneType =
    new NastiInterconnectIO(nMasters, nSlaves).asInstanceOf[this.type]
}

abstract class NastiInterconnect(implicit p: Parameters) extends NastiModule()(p) {
  val nMasters: Int
  val nSlaves: Int

  lazy val io = new NastiInterconnectIO(nMasters, nSlaves)
}

class NastiRecursiveInterconnect(
    val nMasters: Int,
    val nSlaves: Int,
    addrmap: AddrMap,
    base: BigInt = 0)
    (implicit p: Parameters) extends NastiInterconnect {
  var lastEnd = base
  var slaveInd = 0
  val levelSize = addrmap.size
  val realAddrMap = new ArraySeq[(BigInt, BigInt)](addrmap.size)

  addrmap.zipWithIndex.foreach { case (AddrMapEntry(_, startOpt, region), i) =>
    val start = startOpt.getOrElse(lastEnd)
    val size = region.size
    realAddrMap(i) = (start, size)
    lastEnd = start + size
  }

  val flatSlaves = if (nMasters > 1) {
    val xbar = Module(new NastiCrossbar(nMasters, levelSize, realAddrMap))
    xbar.io.masters <> io.masters
    xbar.io.slaves
  } else {
    val router = Module(new NastiRouter(realAddrMap))
    router.io.master <> io.masters.head
    router.io.slave
  }

  addrmap.zip(realAddrMap).zipWithIndex.foreach {
    case ((entry, (start, size)), i) => {
      entry.region match {
        case MemSize(_, _) =>
          io.slaves(slaveInd) <> flatSlaves(i)
          slaveInd += 1
        case MemSubmap(_, submap) =>
          val subSlaves = submap.countSlaves
          val ic = Module(new NastiRecursiveInterconnect(1, subSlaves, submap, start))
          ic.io.masters.head <> flatSlaves(i)
          io.slaves.drop(slaveInd).take(subSlaves).zip(ic.io.slaves).foreach {
            case (s, m) => s <> m
          }
          slaveInd += subSlaves
      }
    }
  }
}

class NastiTopInterconnect(val nMasters: Int, val nSlaves: Int, addrMap: AddrMap)
                          (implicit p: Parameters) extends NastiInterconnect {
  val temp = Module(new NastiRecursiveInterconnect(nMasters, nSlaves, addrMap))

  temp.io.masters.zip(io.masters).foreach { case (t, i) =>
    t.ar <> i.ar
    t.aw <> i.aw
    // this queue is necessary to break up the aw - w dependence
    // introduced by the TileLink -> Nasti converter
    t.w <> Queue(i.w)
    i.b <> t.b
    i.r <> t.r
  }
  //temp.io.masters <> io.masters
  io.slaves <> temp.io.slaves
}
