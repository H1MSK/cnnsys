package lib.quantizer

import spinal.core._

import scala.language.postfixOps

case class RequantizerParamBundle(
    config: RequantizerConfig
) extends Bundle {
  val scale = (config.scale_bitwidth > 0) generate SInt(config.scale_bitwidth bits)
  val offset = config.useOffset generate SInt(config.din_bitwidth + Math.max(config.scale_bitwidth, 0) bits)
  val shift_count = config.useRightShift generate UInt(log2Up(config.din_bitwidth + Math.max(config.scale_bitwidth, 0)) bits)
}
