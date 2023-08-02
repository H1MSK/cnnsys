package lib.quantizer

import spinal.core._

import scala.language.postfixOps

case class RequantizerParamBundle(
    config: RequantizerConfig
) extends Bundle {
  val scale = (config.scale_bitwidth > 0) generate SInt(config.scale_bitwidth bits)
  val offset = config.useOffset generate SInt(config.offset_bitwidth bits)
  val shift_count = config.useRightShift generate UInt(config.shift_bitwidth bits)

  val end_padding = (config.padding_bitwidth != 0) generate Bits(config.padding_bitwidth bits)
}
