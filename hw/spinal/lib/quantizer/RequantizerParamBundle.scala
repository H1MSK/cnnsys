package lib.quantizer

import spinal.core._

import scala.language.postfixOps

case class RequantizerParamBundle(
    config: RequantizerConfig
) extends Bundle {
  val scale = (config.scale_bitwidth > 0) generate SInt(config.scale_bitwidth bits)
  val offset = config.useOffset generate SInt(config.din_bitwidth + Math.max(config.scale_bitwidth, 0) bits)
  val shift_count = config.useRightShift generate UInt(log2Up(config.din_bitwidth + Math.max(config.scale_bitwidth, 0)) bits)

  val end_padding = config.useBundleBytePadding generate {
    var previous_bitwidth = 0
    if (config.scale_bitwidth > 0) previous_bitwidth += scale.getBitsWidth
    if (config.useOffset) previous_bitwidth += offset.getBitsWidth
    if (config.useRightShift) previous_bitwidth += shift_count.getBitsWidth
    (previous_bitwidth % 8 != 0) generate Bits(8 - previous_bitwidth % 8 bits)
  }
}
