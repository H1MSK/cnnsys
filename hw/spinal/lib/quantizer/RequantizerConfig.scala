package lib.quantizer

import spinal.core._

case class RequantizerConfig(
    din_bitwidth: Int,
    dout_bitwidth: Int,
    scale_bitwidth: Int,
    useOffset: Boolean = true,
    useRightShift: Boolean = true,
    useOffsetSaturation: Boolean = true,
    useBundleBytePadding: Boolean = true
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
      log2Up(offset_stage_output_bitwidth + 1)
    else 0

  def shift_stage_output_bitwidth: Int = offset_stage_output_bitwidth

  def bitWidthsToString: String =
    s"$din_bitwidth" +
      (if (useScale) s" -*($scale_bitwidth)> $scale_stage_output_bitwidth" else "") +
      (if (useOffset) s" -+($offset_bitwidth)> $offset_stage_output_bitwidth" else "") +
      (if (useRightShift) s" ->>(${shift_bitwidth})> $shift_stage_output_bitwidth" else "") +
      (if (shift_stage_output_bitwidth != dout_bitwidth) s" -r> $dout_bitwidth" else "")
}
