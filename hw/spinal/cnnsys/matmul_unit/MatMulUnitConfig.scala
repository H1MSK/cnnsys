package cnnsys.matmul_unit

import cnnsys.UnitConfig
import lib.quantizer.RequantizerConfig
import spinal.core.log2Up

case class MatMulUnitConfig() extends UnitConfig {
  var coreInChannelCount: Int = 16
  var coreOutChannelCount: Int = 16

  val coreCount: Int = 1

  var coreInDataBitWidth: Int = 8
  var coreKernelDataBitWidth: Int = 8
  var coreOutDataBitWidth: Int = 8

  def coreMultiplicationResultDataBitWidth: Int = coreInDataBitWidth + coreKernelDataBitWidth

  def coreAddTreeInDataBitWidth: Int = coreMultiplicationResultDataBitWidth
  def coreAddTreeOutDataBitWidth: Int =
    coreAddTreeInDataBitWidth + (if (addTreeExtendBitwidth && !addTreeSaturate) log2Up(coreInChannelCount) else 0)

  def biasDataBitWidth: Int = coreAddTreeInDataBitWidth

  def unitInDataBitWidth: Int = coreInDataBitWidth
  def unitKernelDataBitWidth: Int = coreKernelDataBitWidth
  def unitOutDataBitWidth: Int = coreOutDataBitWidth

  def requantizer_config = RequantizerConfig(
    din_bitwidth = coreAddTreeOutDataBitWidth,
    dout_bitwidth = coreOutDataBitWidth,
    scale_bitwidth = 32 - coreAddTreeInDataBitWidth
  )

  def requantizerScalerDataBitWidth: Int = requantizer_config.scale_bitwidth
  def requantizerOffsetDataBitWidth: Int = requantizer_config.offset_bitwidth
  def requantizerShifterDataBitWidth: Int = requantizer_config.shift_bitwidth


  def inputBusBitWidth: Int = coreCount * coreInChannelCount * unitInDataBitWidth
  def kernelBusBitWidth: Int = coreCount * coreInChannelCount * unitKernelDataBitWidth
  def requantizerBusBitWidth: Int = coreCount * requantizer_config.bundle_bitwidth
  def biasBusBitWidth: Int = coreCount * coreOutChannelCount * biasDataBitWidth
  def outputBusBitWidth: Int = coreCount * coreOutChannelCount * unitOutDataBitWidth

  override def toString: String =
    s"MatMulUnitConfig {\n" +
      s"  coreCount = $coreCount\n" +
      s"  coreInChannelCount = $coreInChannelCount\n" +
      s"  coreOutChannelCount = $coreOutChannelCount\n" +
      "\n" +
      s"  coreInDataBitWidth = $coreInDataBitWidth\n" +
      s"  coreKernelDataBitWidth = $coreKernelDataBitWidth\n" +
      s"  coreOutDataBitWidth = $coreOutDataBitWidth\n" +
      s"  biasDataBitWidth = $biasDataBitWidth\n" +
      s"  requantizerScalerDataBitWidth = $requantizerScalerDataBitWidth\n" +
      s"  requantizerOffsetDataBitWidth = $requantizerOffsetDataBitWidth\n" +
      s"  requantizerShifterDataBitWidth = $requantizerShifterDataBitWidth\n" +
      s"  requantizerPadderDataBitWidth = ${requantizer_config.padding_bitwidth}\n" +
      s"  coreOutDataBitWidth = $coreOutDataBitWidth\n" +
      "\n" +
      s"  inputBusBitWidth = ${coreCount * coreInChannelCount * unitInDataBitWidth}\n" +
      s"  kernelBusBitWidth = ${coreCount * coreInChannelCount * unitKernelDataBitWidth}\n" +
      s"  requantizerBusBitWidth = ${coreCount * requantizer_config.bundle_bitwidth}\n" +
      s"  biasBusBitWidth = ${coreCount * coreOutChannelCount * biasDataBitWidth}\n" +
      s"  outputBusBitWidth = ${coreCount * coreOutChannelCount * unitOutDataBitWidth}\n" +
      "\n" +
      s"  inputBusBitWidth = $inputBusBitWidth\n" +
      s"  kernelBusBitWidth = $kernelBusBitWidth\n" +
      s"  requantizerBusBitWidth = $requantizerBusBitWidth\n" +
      s"  biasBusBitWidth = $biasBusBitWidth\n" +
      s"  outputBusBitWidth = $outputBusBitWidth\n" +
      "}\n"
}

object MatMulUnitConfig {
  val default = MatMulUnitConfig()
}
