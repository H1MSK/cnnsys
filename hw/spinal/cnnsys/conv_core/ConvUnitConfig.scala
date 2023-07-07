package cnnsys.conv_core

import spinal.core._
import spinal.lib.bus.amba4.axilite.AxiLite4Config
import spinal.lib.bus.amba4.axis.Axi4StreamConfig

import scala.language.postfixOps

case class ConvUnitConfig(
                           // Element bit width (Specific data type is not cared here)
                           // 元素位宽（此处不关心具体数据类型）
                           coreInDataBitWidth: Int = 4,
                           coreKernelDataBitWidth: Int = 4,
                           productDataBitWidth: Int = 8,
                           singleChannelSumDataBitWidth: Int = 12,  // 9 of 8 bits
                           sumDataBitWidth: Int = 16,
                           coreOutDataBitWidth: Int = 4,

                           // TODO: Annotation
                           kernelSize: Int = 3,
                           supportedInputWidths: Array[Int] = Array(3, 6, 11, 22),

                           // Dilation and step can be scheduled in software

                           // TODO: Annotation
                           coreCount: Int = 4,
                           coreInChannelCount: Int = 4,
                           coreOutChannelCount: Int = 8,
) {
  val axiLite4Config = AxiLite4Config(
    addressWidth = 10,
    dataWidth = 32
  )

  assert(coreInDataBitWidth * coreInChannelCount * coreCount % 8 == 0)
  val unitInStreamConfig = Axi4StreamConfig(
    dataWidth = coreInDataBitWidth * coreInChannelCount * coreCount / 8,
    idWidth = -1,
    destWidth = -1,
    userWidth = -1,
    useStrb = false,
    useKeep = false,
    useLast = true,
    useId = false,
    useDest = false,
    useUser = false
  )
  val unitOutStreamConfig = unitInStreamConfig.copy(
    dataWidth = coreOutDataBitWidth * coreOutChannelCount * coreCount / 8,
    destWidth = 2,
    useDest = true,
    useLast = false,
    useKeep = true
  )

  val unitParamStreamConfig = Axi4StreamConfig(
    dataWidth = coreKernelDataBitWidth * coreInChannelCount * coreCount / 8,
    idWidth = -1,
    destWidth = -1,
    userWidth = -1,
    useStrb = false,
    useKeep = false,
    useLast = true,
    useId = false,
    useDest = false,
    useUser = false
  )

  if (supportedInputWidths(0) < kernelSize)
    SpinalError("Input width should not be less than kernel size")
  (1 until supportedInputWidths.length).foreach(i => {
    if (supportedInputWidths(i) <= supportedInputWidths(i - 1))
      SpinalError("Supported input widths should be in strict ascent order")
  })

  val maxInputWidth: Int = supportedInputWidths.last

}
