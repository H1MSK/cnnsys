package cnnsys.conv_unit

import cnnsys.UnitConfig
import spinal.core._
import spinal.lib.bus.amba4.axis.Axi4StreamConfig

import scala.language.postfixOps

case class ConvUnitConfig() extends UnitConfig {
  // TODO: Annotation
  val coreCount: Int = 4
  val coreInChannelCount: Int = 8
  val coreOutChannelCount: Int = 8

  // TODO: Annotation
  val kernelSize: Int = 3
  val supportedInputWidths: Array[Int] = Array(3, 6, 11, 22)

  // Element bit width (Specific data type is not cared here)
  // 元素位宽（此处不关心具体数据类型）
  val unitInDataBitWidth: Int = 8
  val unitKernelDataBitWidth: Int = 8
  val coreOutDataBitWidth: Int = 8

  val productDataBitWidth: Int = unitInDataBitWidth + unitKernelDataBitWidth  // 16

  val convAddTreeInputDataBitWidth: Int = productDataBitWidth
  val addTreeRegisterDistance: Int = 2

  val requantizerScalerDataBitWidth: Int = 32
  val requantizerShifterDataBitWidth: Int = 8
  val requantizerInDataBitWidth: Int = 32
  val requantizerOutDataBitWidth: Int = coreOutDataBitWidth

  val biasDataBitWidth: Int = 16  // same as productDataBitWidth

  val coreAdderTreeOutputDataBitWidth: Int = requantizerOutDataBitWidth + log2Up(coreInChannelCount)

  val unitOutDataBitWidth: Int = 8


  val unitInStreamConfig = Axi4StreamConfig(
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
