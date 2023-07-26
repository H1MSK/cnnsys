package cnnsys

import cnnsys.conv_unit.{ConvCore, ConvUnitConfig}
import lib.FragmentRecorder
import lib.quantizer.RequantizerParamBundle
import lib.utils.XilinxBusTagger
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}

import scala.language.postfixOps

case class System() extends Component {
  def genStreamPort(bitCount: BitCount, useLast: Boolean = false) = Axi4Stream(
    Axi4StreamConfig((bitCount.value + 7) / 8, useLast = useLast)
  )

  def connect[T1 <: Data, T2 <: Data](from: Stream[T1], to: Stream[T2]) = {
    to.arbitrationFrom(from)
    to.payload.assignFromBits(from.payload.asBits)
  }

  def connect[T1 <: Data, T2 <: Data](from: Stream[T1], to: Flow[T2]) = {
    to.valid := from.valid
    from.ready := True
    to.payload.assignFromBits(from.payload.asBits)
  }
  val config = ConvUnitConfig()

  val line_width_sel = in Bits (log2Up(config.supportedInputWidths.length) bits)

  val din_stream = slave(genStreamPort(config.unitInDataBitWidth * config.coreInChannelCount bits, useLast = true))
  val kernel_data_stream = slave(genStreamPort(config.unitKernelDataBitWidth * config.coreInChannelCount bits))
  val requantizer_param_stream = slave(genStreamPort(RequantizerParamBundle(config.requantizer_config).getBitsWidth bits))
  val bias_data = slave(genStreamPort(config.biasDataBitWidth * config.coreOutChannelCount bits))
  val dout = master(genStreamPort(config.coreOutDataBitWidth * config.coreOutChannelCount bits, useLast = true))


  val core = ConvCore(config)
  val recorder = FragmentRecorder(Bits(core.din.payload.getBitsWidth bits), Bits(core.dout.payload.getBitsWidth bits))

  recorder.din_frags.arbitrationFrom(din_stream)
  recorder.din_frags.fragment := din_stream.payload.data
  recorder.din_frags.last := din_stream.payload.last

  core.din.arbitrationFrom(recorder.din_stream)
  core.din.payload.assignFromBits(recorder.din_stream.payload)

  recorder.dout_stream.arbitrationFrom(core.dout)
  recorder.dout_stream.payload.assignFromBits(core.dout.payload.asBits)

  dout.arbitrationFrom(recorder.dout_frags)
  dout.payload.data.assignFromBits(recorder.dout_frags.fragment)
  dout.payload.last := recorder.dout_frags.last

  connect(kernel_data_stream, core.kernel_data)
  connect(requantizer_param_stream, core.requantizer_param_in)
  connect(bias_data, core.bias_data)
  core.line_width_sel := line_width_sel

  XilinxBusTagger.tag(din_stream, "din")
  XilinxBusTagger.tag(kernel_data_stream, "kernel_data")
  XilinxBusTagger.tag(requantizer_param_stream, "requantizer_data")
  XilinxBusTagger.tag(bias_data, "bias_data")
  XilinxBusTagger.tag(dout, "dout")
}
