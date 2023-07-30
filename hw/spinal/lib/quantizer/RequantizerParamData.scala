package lib.quantizer

import spinal.core.IntToBuilder
import spinal.core.sim._
import test.TestTask

import scala.language.postfixOps

class RequantizerParamData {
  var scale: Long = 0
  var offset: Long = 0
  var shift_count: Long = 0

  override def toString: String = s"RequantData{ scale=$scale, offset=$offset, shift_count=$shift_count }"

  def simApplyToBundle(bundle: RequantizerParamBundle): Unit = {
    if (bundle.config.scale_bitwidth > 0) {
      assert(TestTask.inRangeOfSInt(bundle.scale.getBitsWidth bits, scale),
        s"scale not in range. Want ${bundle.scale.getBitsWidth} bits, but get $scale\nbit widths:${bundle.config.bitWidthsToString}")
      bundle.scale #= scale
    }
    if (bundle.config.useOffset) {
      assert(TestTask.inRangeOfSInt(bundle.offset.getBitsWidth bits, offset),
        s"offset not in range. Want ${bundle.offset.getBitsWidth} bits, but get $offset\nbit widths:${bundle.config.bitWidthsToString}")
      bundle.offset #= offset
    }
    if (bundle.config.useRightShift) {
      assert(TestTask.inRangeOfUInt(bundle.shift_count.getBitsWidth bits, shift_count),
        s"shift_count not in range. Want ${bundle.shift_count.getBitsWidth} bits, but get $shift_count\nbit widths:${bundle.config.bitWidthsToString}")
      bundle.shift_count #= shift_count
    }
    if (bundle.end_padding != null) bundle.end_padding #= 0
  }
}
