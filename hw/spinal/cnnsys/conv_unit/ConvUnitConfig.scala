package cnnsys.conv_unit

import cnnsys.UnitConfig
import lib.quantizer.RequantizerConfig
import spinal.core._
import spinal.lib.bus.amba4.axis.Axi4StreamConfig

import scala.language.postfixOps

case class ConvUnitConfig() extends UnitConfig {
  // TODO: Annotation
  var coreCount: Int = 4
  var coreInChannelCount: Int = 8
  var coreOutChannelCount: Int = 8

  // TODO: Annotation
  var kernelSize: Int = 3
  var supportedInputWidths: Array[Int] = Array(3, 11, 22)

  // Element bit width (Specific data type is not cared here)
  // 元素位宽（此处不关心具体数据类型）
  var unitInDataBitWidth: Int = 8
  var unitKernelDataBitWidth: Int = 8
  var coreOutDataBitWidth: Int = 8

  def productDataBitWidth: Int = unitInDataBitWidth + unitKernelDataBitWidth  // 16

  def convAddTreeInputDataBitWidth: Int = productDataBitWidth
  def addTreeRegisterDistance: Int = 2

  var requantizerScalerDataBitWidth: Int = 32
  var requantizerShifterDataBitWidth: Int = 8
  var requantizerInDataBitWidth: Int = 32
  def requantizerOutDataBitWidth: Int = coreOutDataBitWidth

  def requantizer_config = RequantizerConfig(
    requantizerInDataBitWidth,
    requantizerOutDataBitWidth,
    requantizerScalerDataBitWidth,
    useOffset = false
  )

  def biasDataBitWidth: Int = productDataBitWidth

  def coreAdderTreeOutputDataBitWidth: Int = requantizerOutDataBitWidth + log2Up(coreInChannelCount)

  def unitOutDataBitWidth: Int = coreOutDataBitWidth

  def unitInStreamConfig = Axi4StreamConfig(
    dataWidth = unitInDataBitWidth * coreInChannelCount * coreCount,
    idWidth = -1,
    destWidth = -1,
    userWidth = -1,
    useStrb = false,
    useKeep = true,
    useLast = true,
    useId = false,
    useDest = false,
    useUser = false
  )
  val unitOutStreamConfig = unitInStreamConfig.copy(
    dataWidth = 1024 / 8,
    destWidth = 2,
    useDest = true,
    useLast = false,
    useKeep = true
  )

  // Dilation and step can be scheduled in software


  assert(unitInDataBitWidth * coreInChannelCount * coreCount % 8 == 0)
  if (supportedInputWidths(0) < kernelSize)
    SpinalError("Input width should not be less than kernel size")
  (1 until supportedInputWidths.length).foreach(i => {
    if (supportedInputWidths(i) <= supportedInputWidths(i - 1))
      SpinalError("Supported input widths should be in strict ascent order")
  })

  val maxInputWidth: Int = supportedInputWidths.last

}

object ConvUnitConfig {
  def default: ConvUnitConfig = ConvUnitConfig()
}
