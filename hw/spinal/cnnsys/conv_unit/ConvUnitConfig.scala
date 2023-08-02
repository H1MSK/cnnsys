package cnnsys.conv_unit

import cnnsys.UnitConfig
import lib.quantizer.RequantizerConfig
import spinal.core._
import spinal.lib.bus.amba4.axis.Axi4StreamConfig

import scala.language.postfixOps

case class ConvUnitConfig() extends UnitConfig {
  var convFlipKernel: Boolean = false
  // TODO: Annotation
  var coreCount: Int = 1

  var coreInChannelCount: Int = 8
  var coreOutChannelCount: Int = 4

  def unitInChannelCount: Int = coreInChannelCount * coreCount

  def unitOutChannelCount: Int = coreOutChannelCount * coreCount

  // TODO: Annotation
  var kernelSize: Int = 3
  var supportedInputWidths: Array[Int] = Array(3, 11, 22)
  var maxPaddingSize: Int = kernelSize / 2

  // Element bit width (Specific data type is not cared here)
  // 元素位宽（此处不关心具体数据类型）
  var coreInDataBitWidth: Int = 8
  var coreKernelDataBitWidth: Int = 8
  var coreOutDataBitWidth: Int = 8

  def unitInDataBitWidth: Int = coreInDataBitWidth

  def unitKernelDataBitWidth: Int = coreKernelDataBitWidth

  def unitOutDataBitWidth: Int = coreOutDataBitWidth

  def coreProductDataBitWidth: Int = coreInDataBitWidth + coreKernelDataBitWidth // 16

  def convAddTreeInputDataBitWidth: Int = coreProductDataBitWidth

  def convAddTreeOutputDataBitWidth: Int = convAddTreeInputDataBitWidth +
    (if (addTreeExtendBitwidth && !addTreeSaturate) log2Up(coreInChannelCount * kernelSize * kernelSize)
    else 0)

  def requantizerInDataBitWidth: Int = convAddTreeOutputDataBitWidth

  var requantizerScalerDataBitWidth: Int = 32 - requantizerInDataBitWidth

  def requantizerOutDataBitWidth: Int = coreOutDataBitWidth

  def requantizer_config = RequantizerConfig(
    requantizerInDataBitWidth,
    requantizerOutDataBitWidth,
    requantizerScalerDataBitWidth,
    useOffset = false,
    useOffsetSaturation = false
  )

  def requantizerShifterDataBitWidth: Int = requantizer_config.shift_bitwidth

  def biasDataBitWidth: Int = coreProductDataBitWidth

  def inputBusBitWidth: Int = coreCount * coreInChannelCount * unitInDataBitWidth
  def kernelBusBitWidth: Int = coreCount * coreInChannelCount * unitKernelDataBitWidth
  def requantizerBusBitWidth: Int = coreCount * requantizer_config.bundle_bitwidth
  def biasBusBitWidth: Int = coreCount * coreOutChannelCount * biasDataBitWidth
  def outputBusBitWidth: Int = coreCount * coreOutChannelCount * unitOutDataBitWidth

  assert(coreInDataBitWidth * coreInChannelCount * coreCount % 8 == 0)
  if (supportedInputWidths(0) < kernelSize)
    SpinalError("Input width should not be less than kernel size")
  (1 until supportedInputWidths.length).foreach(i => {
    if (supportedInputWidths(i) <= supportedInputWidths(i - 1))
      SpinalError("Supported input widths should be in strict ascent order")
  })

  override def toString: String =
    s"ConvUnitConfig {\n" +
      s"  convFlipKernel = $convFlipKernel\n" +
      s"  kernelSize = $kernelSize\n" +
      s"  supportedInputWidths = ${supportedInputWidths.mkString("(", ", ", ")")}\n" +
      s"  maxPaddingSize = $maxPaddingSize\n" +
      "\n" +
      s"  coreCount = $coreCount\n" +
      s"  coreInChannelCount = $coreInChannelCount\n" +
      s"  coreOutChannelCount = $coreOutChannelCount\n" +
      "\n" +
      s"  coreInDataBitWidth = $coreInDataBitWidth\n" +
      s"  coreKernelDataBitWidth = $coreKernelDataBitWidth\n" +
      s"  biasDataBitWidth = $biasDataBitWidth\n" +
      s"  requantizerScalerDataBitWidth = $requantizerScalerDataBitWidth\n" +
      s"  requantizerShifterDataBitWidth = $requantizerShifterDataBitWidth\n" +
      s"  requantizerPadderDataBitWidth = ${requantizer_config.padding_bitwidth}\n" +
      s"  coreOutDataBitWidth = $coreOutDataBitWidth\n" +
      "\n" +
      s"  inputBusBitWidth = $inputBusBitWidth\n" +
      s"  kernelBusBitWidth = $kernelBusBitWidth\n" +
      s"  requantizerBusBitWidth = $requantizerBusBitWidth\n" +
      s"  biasBusBitWidth = $biasBusBitWidth\n" +
      s"  outputBusBitWidth = $outputBusBitWidth\n" +
      "}\n"
}

object ConvUnitConfig {
  def default: ConvUnitConfig = ConvUnitConfig()
}
