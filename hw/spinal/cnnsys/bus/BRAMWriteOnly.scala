package cnnsys.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bram._

case class BRAMWriteOnly(config: BRAMConfig) extends Bundle with IMasterSlave {
  val en = Bool()
  val addr = UInt(config.addressWidth bits)
  val wrdata = Bits(config.dataWidth bits)
  val we = Bits(config.dataWidth / 8 bits)


  override def asMaster(): Unit = {
    out(en, addr, wrdata, we)
  }

  override def asSlave(): Unit = {
    in(en, addr, wrdata, we)
  }

  /**
   * Connect two BRAM bus together Master >> Slave
   */
  def >>(sink: BRAMWriteOnly): Unit = {
    assert(this.config.addressWidth >= sink.config.addressWidth, "BRAM mismatch width address (slave address is bigger than master address )")
    assert(this.config.dataWidth == sink.config.dataWidth, "BRAM mismatch width data (slave and master doesn't have the same data width)")

    sink.addr := this.addr.resized
    sink.we := this.we
    sink.wrdata := this.wrdata
    sink.en := this.en
  }

  def <<(sink: BRAMWriteOnly): Unit = sink >> this
}
