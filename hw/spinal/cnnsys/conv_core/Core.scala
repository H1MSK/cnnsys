package cnnsys.conv_core

import lib.utils.TreeReduce
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Core(config: ConvUnitConfig) extends Component {
  val line_width_sel = in UInt (log2Up(config.supportedInputWidths.length) bits)
  val din = slave Stream Vec(UInt(config.coreInDataBitWidth bits), config.coreInChannelCount)
  val kernel_data = slave Flow Vec(UInt(config.coreKernelDataBitWidth bits), config.coreInChannelCount)
  val dout = master Stream Vec(UInt(config.coreOutDataBitWidth bits), config.coreOutChannelCount)

  private val channels = Array.fill(config.coreInChannelCount)(Channel(config))

  private val addTrees =
    Array.fill(config.coreOutChannelCount)(AddTree(channels.head.dout.payload.head.getBitsWidth, channels.length))

  channels.indices.foreach(i => {
    val c = channels(i)
    c.line_width_sel := line_width_sel
    c.kernel_din.valid := kernel_data.valid
    c.kernel_din.payload := kernel_data.payload(i)

    c.din.payload := din.payload(i)
    c.din.valid := din.valid
    c.dout.ready := addTrees(0).din.ready
  })

  din.ready := channels.head.din.ready

  val add_tree_out_width = addTrees.head.dout.payload.getBitsWidth
  val round_width = add_tree_out_width - config.coreOutDataBitWidth

  (0 until config.coreOutChannelCount).foreach(i => {
    addTrees(i).din.payload := Vec(channels.map(_.dout.payload(i)))
    addTrees(i).din.valid := channels(0).dout.valid

    // TODO: round policy, or dedicate component to dequant
    dout.payload(i) := addTrees(i).dout.payload.round(round_width)
    addTrees(i).dout.ready := dout.ready
  })
  dout.valid := addTrees(0).dout.valid
}
