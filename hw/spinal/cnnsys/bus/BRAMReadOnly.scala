package cnnsys.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bram._

case class BRAMReadOnly(config: BRAMConfig) extends Bundle with IMasterSlave {
  val en = Bool()
  val addr = UInt(config.addressWidth bits)
  val rddata = Bits(config.dataWidth bits)

  override def asMaster(): Unit = {
    out(en, addr)
    in(rddata)
  }

  override def asSlave(): Unit = {
    in(en, addr)
    out(rddata)
  }

  /**
   * Connect two BRAM bus together Master >> Slave
   */
  def >>(sink: BRAMReadOnly): Unit = {
    assert(this.config.addressWidth >= sink.config.addressWidth, "BRAM mismatch width address (slave address is bigger than master address )")
    assert(this.config.dataWidth == sink.config.dataWidth, "BRAM mismatch width data (slave and master doesn't have the same data width)")

    this.rddata := sink.rddata

    sink.addr := this.addr.resized
    sink.en := this.en
  }

  /** Slave << Master */
  def <<(sink: BRAMReadOnly): Unit = sink >> this
}
