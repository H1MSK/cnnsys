package cnnsys.conv_unit

import cnnsys.UnitTrait
import lib.{FragmentRecorder, Input2DPadder}
import lib.quantizer.RequantizerParamBundle
import lib.utils.XilinxBusTagger
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}

import scala.language.postfixOps

case class ConvUnit(config: ConvUnitConfig) extends Component with UnitTrait {
  val line_width_sel =
    (config.supportedInputWidths.length > 1) generate Bits(log2Up(config.supportedInputWidths.length) bits)
  if (config.supportedInputWidths.length > 1) in(line_width_sel)

  val din_stream = slave(
    genStreamPort(config.unitInDataBitWidth * config.coreInChannelCount * config.coreCount bits, useLast = true)
  )
  val kernel_data_stream = slave(
    genStreamPort(config.unitKernelDataBitWidth * config.coreInChannelCount * config.coreCount bits)
  )
  val requantizer_param_stream = slave(
    genStreamPort(RequantizerParamBundle(config.requantizer_config).getBitsWidth * config.coreCount bits)
  )
  val bias_data = slave(genStreamPort(config.biasDataBitWidth * config.coreOutChannelCount * config.coreCount bits))
  val dout = master(
    genStreamPort(config.unitOutDataBitWidth * config.coreOutChannelCount * config.coreCount bits, useLast = true)
  )

  val padding_data = (config.maxPaddingSize > 0) generate SInt(config.unitInDataBitWidth bits)
  val padding_size = (config.maxPaddingSize > 0) generate UInt(log2Up(config.maxPaddingSize + 1) bits)
  if (config.maxPaddingSize > 0) {
    in(padding_data)
    in(padding_size)
  }

  val core = ConvCore(config)

  val padder = (config.maxPaddingSize > 0) generate Input2DPadder(
    Vec(SInt(config.unitInDataBitWidth bits), config.coreInChannelCount * config.coreCount),
    config.supportedInputWidths,
    config.maxPaddingSize
  )

  val recorder = FragmentRecorder(
    Vec(SInt(config.unitInDataBitWidth bits), config.coreInChannelCount * config.coreCount),
    Vec(SInt(config.unitOutDataBitWidth bits), config.coreOutChannelCount * config.coreCount)
  )

  val trimmer = ConvUnitOutputTrimmer(config)

  if (config.maxPaddingSize > 0) {
    padder.din.arbitrationFrom(din_stream)
    padder.din.fragment.assignFromBits(din_stream.payload.data)
    padder.din.last := din_stream.payload.last
    padder.padding_size := padding_size
    padder.padding_data := Vec(padding_data, config.coreInChannelCount * config.coreCount)
    if (config.supportedInputWidths.length > 1)
      padder.line_width_sel := line_width_sel

    recorder.din_frags << padder.dout
  } else {
    recorder.din_frags.arbitrationFrom(din_stream)
    recorder.din_frags.fragment.assignFromBits(din_stream.payload.data)
    recorder.din_frags.last := din_stream.payload.last
  }

  connectStreams(recorder.din_stream, core.din)

  recorder.dout_stream.arbitrationFrom(core.dout)
  recorder.dout_stream.payload.assignFromBits(core.dout.payload.asBits)

  recorder.dout_frags >> trimmer.din
  if (config.supportedInputWidths.length > 1)
    trimmer.line_width_sel := line_width_sel

  dout.arbitrationFrom(trimmer.dout)
  dout.payload.data.assignFromBits(trimmer.dout.fragment.asBits)
  dout.payload.last := trimmer.dout.last

  connectStreamToFlow(kernel_data_stream, core.kernel_data)
  connectStreamToFlow(requantizer_param_stream, core.requantizer_param_in)
  connectStreamToFlow(bias_data, core.bias_data)
  if (config.supportedInputWidths.length > 1)
    core.line_width_sel := line_width_sel

  XilinxBusTagger.tag(din_stream, "din")
  XilinxBusTagger.tag(kernel_data_stream, "kernel_data")
  XilinxBusTagger.tag(requantizer_param_stream, "requantizer_data")
  XilinxBusTagger.tag(bias_data, "bias_data")
  XilinxBusTagger.tag(dout, "dout")
}
