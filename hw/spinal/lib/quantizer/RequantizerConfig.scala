package lib.quantizer

import spinal.core._

case class RequantizerConfig(
    din_bitwidth: Int,
    dout_bitwidth: Int,
    scale_bitwidth: Int,
    useOffset: Boolean = true,
    useRightShift: Boolean = true,
    useOffsetSaturation: Boolean = true,
    useBundleBytePadding: Boolean = true,
    bundlePaddingToPowOf2: Boolean = true
) {
  assert(
    din_bitwidth > 0 &&
      dout_bitwidth > 0 &&
      scale_bitwidth >= 0
  )

  def useScale: Boolean = scale_bitwidth > 0

  def scale_stage_output_bitwidth: Int = din_bitwidth + scale_bitwidth
  def offset_bitwidth: Int =
    if (useOffset)
      scale_stage_output_bitwidth
    else 0

  def offset_stage_output_bitwidth: Int =
    scale_stage_output_bitwidth + (if (!useOffset || useOffsetSaturation) 0 else 1)

  def shift_bitwidth: Int =
    if (useRightShift)
      log2Up(Math.min(offset_stage_output_bitwidth, dout_bitwidth))
    else 0

  def shift_stage_output_bitwidth: Int = offset_stage_output_bitwidth

  def padding_bitwidth: Int = if (!useBundleBytePadding) 0
  else {
    val previous_bitwidth = scale_bitwidth + offset_bitwidth + shift_bitwidth
    if (!bundlePaddingToPowOf2) {
      if (previous_bitwidth % 8 != 0) 8 - previous_bitwidth % 8
      else 0
    } else {
      val total_bitwidth = 1 << log2Up(previous_bitwidth)
      total_bitwidth - previous_bitwidth
    }
  }

  def hasRoundStage: Boolean = shift_stage_output_bitwidth != dout_bitwidth

  def bundle_bitwidth: Int = scale_bitwidth + offset_bitwidth + shift_bitwidth + padding_bitwidth

  def bitWidthsToString: String =
    s"$din_bitwidth" +
      (if (useScale) s" -*($scale_bitwidth)> $scale_stage_output_bitwidth" else "") +
      (if (useOffset) s" -+($offset_bitwidth)> $offset_stage_output_bitwidth" else "") +
      (if (useRightShift) s" ->>(${shift_bitwidth})> $shift_stage_output_bitwidth" else "") +
      (if (shift_stage_output_bitwidth != dout_bitwidth) s" -r> $dout_bitwidth" else "")
}
