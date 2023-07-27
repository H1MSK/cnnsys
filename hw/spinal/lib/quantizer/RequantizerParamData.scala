package lib.quantizer

import spinal.core.sim._

class RequantizerParamData {
  var scale: Int = 0
  var offset: Int = 0
  var shift_count: Int = 0

  def simApplyToBundle(bundle: RequantizerParamBundle): Unit = {
    if (bundle.config.scale_bitwidth > 0) bundle.scale #= scale
    if (bundle.config.useOffset) bundle.offset #= offset
    if (bundle.config.useRightShift) bundle.shift_count #= shift_count
    if (bundle.end_padding != null) bundle.end_padding #= 0
  }
}
