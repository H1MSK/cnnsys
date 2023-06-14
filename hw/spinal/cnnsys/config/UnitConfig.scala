package cnnsys.config

import spinal.core._

case class UnitConfig (
  // Element bit width (Specific data type is not cared here)
  // 元素位宽（此处不关心具体数据类型）
  inDataBitWidth: Int = 4,
  outDataBitWidth: Int = 4,
  kernelDataBitWidth: Int = 4,

  // TODO: Annotation
  maxKernelSize: Int = 3,
  maxInputWidth: Int = 40,
  maxPadding: Int = 2,
  maxDilation: Int = 2,

  // TODO: Annotation
  coreCount: Int = 4,
  coreInChannelCount: Int = 32,
  coreOutChannelCount: Int = 4,

  bramSizeMultiplier: Int = 2,

  // TODO: Annotation
  globalDataBitWidth: Int = 32,
  globalDataAddressWidth: Int = 32,

) {
  // TODO: Annotation
  val coreInBramDataWidth: Int = inDataBitWidth * coreInChannelCount
  val coreOutBramDataWidth: Int = outDataBitWidth * coreOutChannelCount
  val coreParamBramDataWidth: Int = kernelDataBitWidth * coreInChannelCount * coreOutBramDataWidth

  // TODO: Annotation
  val coreInBramSize: Int = maxInputWidth * maxInputWidth * bramSizeMultiplier
  val coreOutBramSize: Int = maxInputWidth * maxInputWidth * bramSizeMultiplier
  val coreParamBramSize: Int = maxKernelSize * maxKernelSize * bramSizeMultiplier

  // TODO: Annotation
  val coreInBramAddrWidth: Int = log2Up(coreInBramSize)
  val coreOutBramAddrWidth: Int = log2Up(coreOutBramSize)
  val coreParamBramAddrWidth: Int = log2Up(coreParamBramSize)
}
