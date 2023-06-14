package cnnsys.blackbox

import cnnsys.ProjectConfig
import cnnsys.bus.{BRAMReadOnly, BRAMWriteOnly}
import spinal.core._
import spinal.lib.bus.bram._

case class MemSdp(rd_count: Int, rd_width: Int, wr_width: Int, use_32bit_addr: Boolean = false) extends BlackBox {
  if (wr_width > rd_width) {
    if(wr_width % rd_width != 0 || rd_count * rd_width % wr_width != 0) {
      SpinalError("Mem_sdp : Read write params mismatched")
    }
  } else if (wr_width < rd_width) {
    if (rd_width % wr_width != 0) {
      SpinalError("Mem_sdp : Read write params mismatched")
    }
  }

  addGeneric("rd_count", rd_count)
  addGeneric("rd_width", rd_width)
  addGeneric("wr_width", wr_width)
  addGeneric("use_32bit_addr", use_32bit_addr)

  val io = new Bundle {
    val clk = in Bool()
    val wr = BRAMWriteOnly(BRAMConfig(
      dataWidth = wr_width,
      addressWidth = if (use_32bit_addr) 32 else log2Up(rd_count * rd_width / wr_width)
    ))
    val rd = BRAMReadOnly(BRAMConfig(
      dataWidth = rd_width,
      addressWidth = if (use_32bit_addr) 32 else log2Up(rd_count)
    ))
  }

  io.rd.setAsSlave()

  io.wr.setAsSlave()

  mapCurrentClockDomain(io.clk)

  noIoPrefix()

  private def renameIO(): Unit = {
    io.clk.setName("clka")
    io.wr.en.setName("ena")
    io.wr.addr.setName("addra")
    io.wr.wrdata.setName("dina")
    io.wr.we.setName("wea")

    io.rd.addr.setName("addrb")
    io.rd.rddata.setName("doutb")
    io.rd.en.setName("enb")
  }
}

case class MemTestTop() extends Component {
  val m = MemSdp(32, 16, 64, use_32bit_addr = false)
  val wr = BRAMWriteOnly(BRAMConfig(
    dataWidth = 64,
    addressWidth = 3
  ))
  val rd = BRAMReadOnly(BRAMConfig(
    dataWidth = 16,
    addressWidth = 5
  ))
  wr.setAsSlave()
  rd.setAsSlave()

  wr >> m.io.wr
  rd >> m.io.rd
}

object MemGen extends App {
  ProjectConfig.spinal.generateVerilog(MemTestTop()).printPruned()
}