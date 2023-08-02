package cnnsys.matmul_unit

import cnnsys.UnitTrait
import lib.FragmentRecorder
import lib.quantizer.RequantizerParamBundle
import lib.utils.XilinxBusTagger
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class MatMulUnit(config: MatMulUnitConfig) extends Component with UnitTrait {
  val din_stream = slave(genStreamPort(config.coreInDataBitWidth * config.coreInChannelCount bits, useLast = true))
  val kernel_data_stream = slave(genStreamPort(config.coreInDataBitWidth * config.coreInChannelCount bits))
  val requantizer_param_stream = slave(
    genStreamPort(RequantizerParamBundle(config.requantizer_config).getBitsWidth bits)
  )
  val bias_data = slave(genStreamPort(config.biasDataBitWidth * config.coreOutChannelCount bits))

  val dout = master(slave(genStreamPort(config.coreOutDataBitWidth * config.coreOutChannelCount bits, useLast = true)))

  val core = MatMulCore(config)
  val recorder = FragmentRecorder(
    Vec(SInt(config.coreInDataBitWidth bits), config.coreInChannelCount),
    Vec(SInt(config.coreOutDataBitWidth bits), config.coreOutChannelCount)
  )

  connectStreamToFlow(kernel_data_stream, core.kernel_data)
  connectStreamToFlow(requantizer_param_stream, core.requantizer_param_in)
  connectStreamToFlow(bias_data, core.bias_data)

  recorder.din_frags.arbitrationFrom(din_stream)
  recorder.din_frags.fragment.assignFromBits(din_stream.data)
  recorder.din_frags.last := din_stream.last

  connectStreams(recorder.din_stream, core.din)

  connectStreams(core.dout, recorder.dout_stream)

  dout.arbitrationFrom(recorder.dout_frags)
  dout.data.assignFromBits(recorder.dout_frags.fragment.asBits)
  dout.last := recorder.dout_frags.last

  XilinxBusTagger.tag(din_stream, "din")
  XilinxBusTagger.tag(kernel_data_stream, "kernel_data")
  XilinxBusTagger.tag(requantizer_param_stream, "requantizer_data")
  XilinxBusTagger.tag(bias_data, "bias_data")
  XilinxBusTagger.tag(dout, "dout")
}
