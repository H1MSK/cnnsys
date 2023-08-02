package cnnsys

import cnnsys.conv_unit.{ConvCore, ConvUnit, ConvUnitConfig}
import cnnsys.matmul_unit.{MatMulUnit, MatMulUnitConfig}

import scala.language.postfixOps

object GenConvCoreVerilog extends App {
  ProjectConfig.spinal.generateVerilog(ConvCore(ConvUnitConfig.default)).printPruned()
}

object GenConvUnitVerilog extends App {
  ProjectConfig.spinal.generateVerilog(ConvUnit(ConvUnitConfig.default)).printPruned()
}

object GenMatMulUnitVerilog extends App {
  ProjectConfig.spinal.generateVerilog(MatMulUnit(MatMulUnitConfig.default)).printPruned()
}

object GenSystemVerilog extends App {
  ProjectConfig.spinal.generateVerilog(System()).printPruned()
}
