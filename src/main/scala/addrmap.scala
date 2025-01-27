// See LICENSE for license details.

package junctions

import Chisel._
import scala.collection.mutable.HashMap

case object PAddrBits extends Field[Int]
case object VAddrBits extends Field[Int]
case object PgIdxBits extends Field[Int]
case object PgLevels extends Field[Int]
case object PgLevelBits extends Field[Int]
case object ASIdBits extends Field[Int]
case object PPNBits extends Field[Int]
case object VPNBits extends Field[Int]

case object GlobalAddrMap extends Field[AddrMap]
case object MMIOBase extends Field[BigInt]

trait HasAddrMapParameters {
  implicit val p: Parameters

  val paddrBits = p(PAddrBits)
  val vaddrBits = p(VAddrBits)
  val pgIdxBits = p(PgIdxBits)
  val ppnBits = p(PPNBits)
  val vpnBits = p(VPNBits)
  val pgLevels = p(PgLevels)
  val pgLevelBits = p(PgLevelBits)
  val asIdBits = p(ASIdBits)

  val addrMap = new AddrHashMap(p(GlobalAddrMap))
}

abstract class MemRegion { def size: BigInt }

case class MemSize(size: BigInt, prot: Int) extends MemRegion

case class MemSubmap(size: BigInt, entries: AddrMap) extends MemRegion

object AddrMapConsts {
  val R = 0x4
  val W = 0x2
  val X = 0x1
  val RW = R | W
  val RX = R | X
  val RWX = R | W | X
}

class AddrMapProt extends Bundle {
  val r = Bool()
  val w = Bool()
  val x = Bool()
}

case class AddrMapEntry(name: String, start: Option[BigInt], region: MemRegion)

case class AddrHashMapEntry(port: Int, start: BigInt, size: BigInt, prot: Int)

class AddrMap(entries: Seq[AddrMapEntry]) extends scala.collection.IndexedSeq[AddrMapEntry] {
  
  def apply(index: Int): AddrMapEntry = entries(index)

  def length: Int = entries.size

  def countSlaves: Int = {
    this map { entry: AddrMapEntry => entry.region match {
      case MemSize(_, _) => 1
      case MemSubmap(_, submap) => submap.countSlaves
    }} reduceLeft(_ + _)
  }
}

object AddrMap {
  def apply(elems: AddrMapEntry*): AddrMap = new AddrMap(elems)
}

class AddrHashMap(addrmap: AddrMap) {
  val mapping = new HashMap[String, AddrHashMapEntry]

  private def genPairs(am: AddrMap): Seq[(String, AddrHashMapEntry)] = {
    var ind = 0
    var base = BigInt(0)
    var pairs = Seq[(String, AddrHashMapEntry)]()
    am.foreach { case AddrMapEntry(name, startOpt, region) =>
      region match {
        case MemSize(size, prot) => {
          if (!startOpt.isEmpty) base = startOpt.get
          pairs = (name, AddrHashMapEntry(ind, base, size, prot)) +: pairs
          base += size
          ind += 1
        }
        case MemSubmap(size, submap) => {
          if (!startOpt.isEmpty) base = startOpt.get
          val subpairs = genPairs(submap).map {
            case (subname, AddrHashMapEntry(subind, subbase, subsize, prot)) =>
              (name + ":" + subname,
                AddrHashMapEntry(ind + subind, base + subbase, subsize, prot))
          }
          pairs = subpairs ++ pairs
          ind += subpairs.size
          base += size
        }
      }
    }
    pairs
  }

  for ((name, ind) <- genPairs(addrmap)) { mapping(name) = ind }

  def nEntries: Int = mapping.size
  def apply(name: String): AddrHashMapEntry = mapping(name)
  def get(name: String): Option[AddrHashMapEntry] = mapping.get(name)
  def sortedEntries(): Seq[(String, BigInt, BigInt, Int)] = {
    val arr = new Array[(String, BigInt, BigInt, Int)](mapping.size)
    mapping.foreach { case (name, AddrHashMapEntry(port, base, size, prot)) =>
      arr(port) = (name, base, size, prot)
    }
    arr.toSeq
  }

  def isValid(addr: UInt): Bool = {
    sortedEntries().map { case (_, base, size, _) =>
      addr >= UInt(base) && addr < UInt(base + size)
    }.reduceLeft(_ || _)
  }

  def getProt(addr: UInt): AddrMapProt = {
    Mux1H(sortedEntries().map { case (_, base, size, prot) =>
      (addr >= UInt(base) && addr < UInt(base + size),
        new AddrMapProt().fromBits(Bits(prot, 3)))
    })
  }
}
