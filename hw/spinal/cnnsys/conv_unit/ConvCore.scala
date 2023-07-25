package cnnsys.conv_unit

import lib.quantizer.{Requantizer, RequantizerChain, RequantizerConfig, RequantizerParamBundle}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ConvCore(config: ConvUnitConfig) extends Component {
  val line_width_sel = in Bits (log2Up(config.supportedInputWidths.length) bits)
  val din = slave Stream Vec(SInt(config.unitInDataBitWidth bits), config.coreInChannelCount)
  val kernel_data = slave Flow Vec(SInt(config.unitKernelDataBitWidth bits), config.coreInChannelCount)
  val requantizer_param_in = slave Flow RequantizerParamBundle(config.requantizer_config)
  val bias_data = slave Flow Vec(SInt(config.biasDataBitWidth bits), config.coreOutChannelCount)
  val dout = master Stream Vec(SInt(config.coreOutDataBitWidth bits), config.coreOutChannelCount)

  private val reg_bias = Vec(bias_data.payload.map(RegNextWhen(_, bias_data.valid, init = S(0))))

  private val channels = Array.fill(config.coreInChannelCount)(ConvChannel(config))

  private val finalRequantizerChain = RequantizerChain(
    chain_length = config.coreOutChannelCount,
    enableChainOut = false,
    parallel_count = 1,
    config = config.requantizer_config
  )
  finalRequantizerChain.param << requantizer_param_in

  private val addTrees =
    Array.fill(config.coreOutChannelCount)(
      ConvAddTree(
        input_bit_width = config.convAddTreeInputDataBitWidth,
        length = config.coreInChannelCount * config.kernelSize * config.kernelSize,
        register_distance = config.addTreeRegisterDistance,
        extend_bitwidth = false,
        use_bias = true
      )
    )

  channels.indices.foreach(i => {
    val c = channels(i)
    c.line_width_sel := line_width_sel
    c.kernel_din.valid := kernel_data.valid
    c.kernel_din.payload := kernel_data.payload(i)

    c.din.payload := din.payload(i)
    c.din.valid := din.valid
  })

  din.ready := channels.head.din.ready

  channels.foreach(_.dout.ready := addTrees(0).din.ready)

  (0 until config.coreOutChannelCount).foreach(i => {
    addTrees(i).din.payload := Vec(channels.flatMap(_.dout.payload(i)).toSeq)
    addTrees(i).din.valid := channels(0).dout.valid
    addTrees(i).bias := reg_bias(i)
    addTrees(i).dout.ready := finalRequantizerChain.din.ready
  })

  (0 until config.coreOutChannelCount).foreach(i =>
    finalRequantizerChain.din.payload(i)(0) := addTrees(i).dout.payload.resized
  )
  finalRequantizerChain.din.valid := addTrees(0).dout.valid
  dout.arbitrationFrom(finalRequantizerChain.dout)
  dout.payload := Vec(finalRequantizerChain.dout.payload.flatten[SInt])
}
