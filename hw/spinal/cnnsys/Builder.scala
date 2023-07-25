package cnnsys

import cnnsys.conv_unit.{ConvChannel, ConvUnitConfig, ConvCore, ConvUnit}
import spinal.core.IntToBuilder

import scala.language.postfixOps

object GenChannelVerilog extends App {
  ProjectConfig.spinal.generateVerilog(ConvChannel(ConvUnitConfig())).printPruned()
}

object GenCoreVerilog extends App {
  ProjectConfig.spinal.generateVerilog(ConvCore(ConvUnitConfig())).printPruned()
}

object GenUnitVerilog extends App {
  ProjectConfig.spinal.generateVerilog(ConvUnit(ConvUnitConfig(), sizeMapping = (0x0000, 1 KiB))).printPruned()
}

object GenSystemVerilog extends App {
  ProjectConfig.spinal.generateVerilog(System()).printPruned()
}
