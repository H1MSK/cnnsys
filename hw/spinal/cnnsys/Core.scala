package cnnsys

import cnnsys.bus.{BRAMReadOnly, BRAMWriteOnly}
import cnnsys.config.UnitConfig
import spinal.core._
import spinal.lib.Delay
import spinal.lib.bus.bram._
import utils.TreeReduce

case class Core(config: UnitConfig) extends Component {
  val line_width = in UInt(log2Up(config.maxInputWidth) bits)
  val param_bram = BRAMReadOnly(BRAMConfig(
    dataWidth = config.coreParamBramDataWidth,
    addressWidth = config.coreParamBramAddrWidth
  ))
  param_bram.setAsMaster()
  val in_bram = BRAMReadOnly(BRAMConfig(
    dataWidth = config.coreInBramDataWidth,
    addressWidth = config.coreInBramAddrWidth
  ))
  in_bram.setAsMaster()
  val out_bram = BRAMWriteOnly(BRAMConfig(
    dataWidth = config.coreOutBramDataWidth,
    addressWidth = config.coreOutBramAddrWidth
  ))
  out_bram.setAsMaster()

  val in_ptr = in UInt (config.coreInBramAddrWidth bits)
  val out_ptr = in UInt (config.coreOutBramAddrWidth bits)

  val param_ptr = in UInt(config.coreParamBramAddrWidth bits)
  val param_en = in Bool()
  val in_en = in Bool()

  val channels = Array.fill(config.coreInChannelCount)(Channel(config))

  param_bram.addr := param_ptr
  param_bram.en := param_en

  val kernel_en = Delay(param_en, 1)
  val kernel_data = param_bram.rddata
  channels.indices.foreach(i => {
    val c = channels(i)
    c.kernel_shift_en := kernel_en
    c.line_width := line_width
    c.in_shift_kernel := kernel_data(
      i * config.kernelDataBitWidth * config.coreOutChannelCount,
      config.kernelDataBitWidth * config.coreOutChannelCount bits).asUInt
  })

  in_bram.addr := in_ptr
  in_bram.en := in_en

  val in_en_reg = RegNext(in_en)
  channels.foreach(c => c.en := in_en_reg)

  val in_data = in_bram.rddata
  channels.indices.foreach(i => {
    val in_data_slice = in_data(i * config.inDataBitWidth, config.inDataBitWidth bits).asUInt
    channels(i).in_data := in_data_slice
  })

  val out_data = UInt(config.outDataBitWidth * config.coreOutChannelCount bits)

  out_data := TreeReduce(channels.map(_.out_data).toSeq, (a: UInt, b: UInt) => a + b)

  out_bram.en := Delay(in_en, channels(0).delay + 1)
  out_bram.we := (default -> true)
  out_bram.wrdata := out_data.asBits
  out_bram.addr := Delay(out_ptr, channels(0).delay + 1)
}
