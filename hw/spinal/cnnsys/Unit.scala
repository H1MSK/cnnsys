package cnnsys

import cnnsys.blackbox.MemSdp
import cnnsys.bus._
import cnnsys.config.UnitConfig
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config}
import spinal.lib.bus.regif.{AccessType, BusInterface}

case class Unit(config: UnitConfig) extends Component {
  val io_ctrl = AxiLite4(AxiLite4Config(dataWidth = 32, addressWidth = 16))
//  val io_data = Axi4(Axi4Config(
//    dataWidth = config.globalDataBitWidth,
//    addressWidth = config.globalDataAddressWidth,
//    idWidth = 0))

  io_ctrl.setAsSlave()
//  io_data.setAsMaster()

  val ctrl_busif = BusInterface(io_ctrl, (0x000, 256 Bytes))

  val ctrl_main = ctrl_busif.newReg("Control register")

  val in_info = ctrl_busif.newRegAt(0x10, "Input Information")
  val in_width = in_info.field(UInt(log2Up(config.maxInputWidth) bits), AccessType.RW, 0, "Input width")
  val in_height = in_info.field(UInt(log2Up(config.maxInputWidth) bits), AccessType.RW, 0, "Input height")
  val in_chan = in_info.field(UInt(log2Up(config.coreInChannelCount * config.coreCount) bits), AccessType.RW, 0, "Input channel count")
  val pad = ctrl_busif.newReg("Padding Size")
  val stride = ctrl_busif.newReg("Stride Size")
  val dilation = ctrl_busif.newReg("Dilation Size")
  val kn_size = ctrl_busif.newReg("Kernel Side Length")

  val in_addr_start = ctrl_busif.newRegAt(0x80, "Input data start address")
  val in_addr_hstep = ctrl_busif.newReg("Input data horizontal offset")
  val in_addr_vstep = ctrl_busif.newReg("Input data vertical offset")

  val kern_addr_start = ctrl_busif.newReg("Kernel data start address")
  val kern_addr_hstep = ctrl_busif.newReg("Kernel data horizontal offset")
  val kern_addr_vstep = ctrl_busif.newReg("Kernel data vertical offset")

  val cores = Seq.fill(config.coreCount)(Core(config))

  val inbufs = Seq.fill(config.coreCount)(
    MemSdp(
      config.coreInBramSize,
      config.coreInBramDataWidth,
      config.coreInBramDataWidth
    ))

  val outbufs = Seq.fill(config.coreCount)(
    MemSdp(
      config.coreOutBramSize,
      config.coreOutBramDataWidth,
      config.coreOutBramDataWidth
    ))

  val parambufs = Seq.fill(config.coreCount)(
    MemSdp(
      config.coreParamBramSize,
      config.coreParamBramDataWidth,
      config.coreParamBramDataWidth
    ))

  val inbuf_wr_ports = inbufs.map(m => {
    val in_wr = BRAMWriteOnly(m.io.wr.config)
    in_wr.setAsSlave()
    in_wr >> m.io.wr
    in_wr
  })
  val outbuf_rd_ports = outbufs.map(m => {
    val out_rd = BRAMReadOnly(m.io.wr.config)
    out_rd.setAsSlave()
    out_rd >> m.io.rd
    out_rd
  })
  val parambuf_wr_ports = parambufs.map(m => {
    val param_wr = BRAMWriteOnly(m.io.wr.config)
    param_wr.setAsSlave()
    param_wr >> m.io.wr
    param_wr
  })

  out(Vec(inbuf_wr_ports))
  out(Vec(outbuf_rd_ports))
  out(Vec(parambuf_wr_ports))

  val core_in_en = RegInit(Bool(false))
  val core_param_en = RegInit(Bool(false))
  val core_in_ptr = RegInit(U(0, config.coreInBramAddrWidth bits))
  val core_out_ptr = RegInit(U(0, config.coreOutBramAddrWidth bits))
  val core_param_ptr = RegInit(U(0, config.coreParamBramAddrWidth bits))

  cores.indices.foreach(i => {
    val c = cores(i)
    c.line_width := in_width

    c.in_en := core_in_en
    c.param_en := core_param_en

    c.in_ptr := core_in_ptr
    c.out_ptr := core_out_ptr
    c.param_ptr := core_param_ptr

    c.in_bram >> inbufs(i).io.rd
    c.param_bram >> parambufs(i).io.rd
    c.out_bram >> outbufs(i).io.wr
  })
}
