package cnnsys

import cnnsys.conv_core.{Channel, ConvUnitConfig, Core, Unit}
import spinal.core.IntToBuilder

import scala.language.postfixOps

object GenChannelVerilog extends App {
  ProjectConfig.spinal.generateVerilog(Channel(ConvUnitConfig())).printPruned()
}

object GenCoreVerilog extends App {
  ProjectConfig.spinal.generateVerilog(Core(ConvUnitConfig())).printPruned()
}

object GenUnitVerilog extends App {
  ProjectConfig.spinal.generateVerilog(Unit(ConvUnitConfig(), sizeMapping = (0x0000, 1 KiB))).printPruned()
}
