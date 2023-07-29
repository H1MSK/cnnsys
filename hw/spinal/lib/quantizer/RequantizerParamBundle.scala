package lib.quantizer

import spinal.core._

import scala.language.postfixOps

case class RequantizerParamBundle(
    config: RequantizerConfig
) extends Bundle {
  val scale = (config.scale_bitwidth > 0) generate SInt(config.scale_bitwidth bits)
  val offset = config.useOffset generate SInt(config.offset_bitwidth bits)
  val shift_count = config.useRightShift generate UInt(config.shift_bitwidth bits)

  val end_padding = config.useBundleBytePadding generate {
    var previous_bitwidth = config.offset_bitwidth + config.shift_bitwidth
    if (config.scale_bitwidth > 0) previous_bitwidth += scale.getBitsWidth
    (previous_bitwidth % 8 != 0) generate Bits(8 - previous_bitwidth % 8 bits)
  }
}
