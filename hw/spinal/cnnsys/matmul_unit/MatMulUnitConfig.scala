package cnnsys.matmul_unit

import cnnsys.UnitConfig
import lib.quantizer.RequantizerConfig
import spinal.core.log2Up

case class MatMulUnitConfig() extends UnitConfig {
  var inputWidth: Int = 16
  var outputWidth: Int = 16

  var coreInDataBitWidth: Int = 8
  var coreKernelDataBitWidth: Int = 8
  var coreOutDataBitWidth: Int = 8

  def coreMultiplicationResultDataBitWidth: Int = coreInDataBitWidth + coreKernelDataBitWidth

  def coreAddTreeInDataBitWidth: Int = coreMultiplicationResultDataBitWidth
  def coreAddTreeOutDataBitWidth: Int =
    coreAddTreeInDataBitWidth + (if (addTreeExtendBitwidth && !addTreeSaturate) log2Up(inputWidth) else 0)

  def biasDataBitWidth: Int = coreAddTreeInDataBitWidth

  def unitInDataBitWidth: Int = coreInDataBitWidth
  def unitKernelDataBitWidth: Int = coreKernelDataBitWidth
  def unitOutDataBitWidth: Int = coreOutDataBitWidth

  def requantizer_config = RequantizerConfig(
    din_bitwidth = coreAddTreeOutDataBitWidth,
    dout_bitwidth = coreOutDataBitWidth,
    scale_bitwidth = 32 - coreAddTreeInDataBitWidth
  )
}

object MatMulUnitConfig {
  val default = MatMulUnitConfig()
}
