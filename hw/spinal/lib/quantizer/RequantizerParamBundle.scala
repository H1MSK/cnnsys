package lib.quantizer

import spinal.core._

import scala.language.postfixOps

case class RequantizerParamBundle(
    config: RequantizerConfig
) extends Bundle {
  val scale = SInt(config.scale_bitwidth bits)
  val offset = config.useOffset generate SInt(config.din_bitwidth + config.scale_bitwidth bits)
  val shift_count = config.useRightShift generate UInt(log2Up(config.dout_bitwidth) bits)
}
