package cnnsys

import cnnsys.conv_unit.{ConvCore, ConvUnitConfig}
import lib.quantizer.RequantizerParamBundle
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}

import scala.language.postfixOps

case class System() extends Component {
  val config = ConvUnitConfig()

  val core = ConvCore(config)

  val line_width_sel = in Bits (log2Up(config.supportedInputWidths.length) bits)

  def genStreamPort(bitCount: BitCount) = Axi4Stream(Axi4StreamConfig((bitCount.value + 7) / 8))

  val din_stream = slave(genStreamPort(config.unitInDataBitWidth * config.coreInChannelCount bits))
  val kernel_data_stream = slave(genStreamPort(config.unitKernelDataBitWidth * config.coreInChannelCount bits))
  val requantizer_param_stream = slave(genStreamPort(RequantizerParamBundle(config.requantizer_config).getBitsWidth bits))
  val bias_data = slave(genStreamPort(config.biasDataBitWidth * config.coreOutChannelCount bits))
  val dout = master(genStreamPort(config.coreOutDataBitWidth * config.coreOutChannelCount bits))

  def connect[T1 <: Data, T2 <: Data](from: Stream[T1], to: Stream[T2]) = {
    to.arbitrationFrom(from)
    to.payload.assignFromBits(from.payload.asBits)
  }
  def connect[T1 <: Data, T2 <: Data](from: Stream[T1], to: Flow[T2]) = {
    to.valid := from.valid
    from.ready := True
    to.payload.assignFromBits(from.payload.asBits)
  }
  connect(din_stream, core.din)
  connect(kernel_data_stream, core.kernel_data)
  connect(requantizer_param_stream, core.requantizer_param_in)
  connect(bias_data, core.bias_data)
  connect(core.dout, dout)
  core.line_width_sel := line_width_sel
}
