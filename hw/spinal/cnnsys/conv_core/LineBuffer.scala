package cnnsys.conv_core

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

case class LineBuffer(config: ConvUnitConfig, hasShiftOutput: Boolean = true)
    extends Component {
  if (config.supportedInputWidths(0) < config.kernelSize)
    SpinalError("Input width should not be smaller than kernel size")

  val din = slave Flow UInt(config.coreInDataBitWidth bits)

  val line_width_sel = hasShiftOutput generate UInt (log2Up(config.supportedInputWidths.length) bits)

  val dout = hasShiftOutput generate UInt(config.coreInDataBitWidth bits)

  private val zero = U(0, din.payload.getBitsWidth bits)
  private val history = History(
    RegNextWhen(din.payload, din.valid, init = zero),
    if(hasShiftOutput) config.maxInputWidth else config.kernelSize,
    when = din.valid,
    init = zero
  )

  val exports = Vec(history.slice(0, config.kernelSize))
  out(exports)

  if (hasShiftOutput) {
    in(line_width_sel)
    out(dout)

    dout := line_width_sel.muxList(
      config.supportedInputWidths.indices.map(i => (i, history(config.supportedInputWidths(i) - 1)))
    )
  }

  val delay = 1
}
