package cnnsys.conv_unit

import spinal.core._

import scala.language.postfixOps

object ConvUnitCoreCooperateMode extends SpinalEnum {
  val independent, pairExtend, fullExtend = newElement()
}

case class ConvUnitTask (config: ConvUnitConfig = ConvUnitConfig.default) extends Bundle {
  val load_param = Bool()
  val width_sel = Bits(log2Up(config.supportedInputWidths.length) bits)
  val dest = UInt(config.unitOutStreamConfig.destWidth bits)
  val do_mul_sigmoid = Bool()
  val coop_mode = ConvUnitCoreCooperateMode()
}
