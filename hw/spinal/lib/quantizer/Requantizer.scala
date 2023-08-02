package lib.quantizer

import lib.StreamController.StreamController
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Requantizer(
    parallel_count: Int,
    config: RequantizerConfig
    // TODO: round policy as parameter
) extends Component {
  val din = slave Stream Vec(SInt(config.din_bitwidth bits), parallel_count)
  val dout = master Stream Vec(SInt(config.dout_bitwidth bits), parallel_count)
  val param = in(RequantizerParamBundle(config))

  var current_val = din.payload

  // Do not collapse bubble to infer DSP resource
  val controller = StreamController(config.stage_count, collapse_bubble = false)
  controller << din
  controller >> dout
  var current_stage_count = 0

  if (config.useScale) {
    val scaled = Vec(current_val.map(_ * param.scale)).setName("scaled")
    val reg_scaled = RegNextWhen(scaled, controller.en(current_stage_count), scaled.getZero).setName("reg_scaled")
    current_val = reg_scaled
    current_stage_count += 1
    assert(current_val.head.getBitsWidth == config.scale_stage_output_bitwidth)
  }

  if (config.useOffset) {
    val offset = Vec(
      current_val.map(_ +^ param.offset)
    ).setName("offset")
    val reg_offset =
      RegNextWhen(offset, controller.en(current_stage_count), init = offset.getZero).setName("reg_offset")

    // split add and saturate to infer DSP resource
    current_val = if (config.useOffsetSaturation) {
      Vec(reg_offset.map(_.sat(1)))
    } else {
      reg_offset
    }
    current_stage_count += 1
    assert(current_val.head.getBitsWidth == config.offset_stage_output_bitwidth)
  }

  if (config.useRightShift) {
    val shifted = Vec(current_val.map(_ >> param.shift_count)).setName("shifted")
    val reg_shifted =
      RegNextWhen(shifted, controller.en(current_stage_count), init = shifted.getZero).setName("reg_shifted")
    current_val = reg_shifted
    shifted.setName("shifted")
    current_stage_count += 1
  }

  assert(current_val.head.getBitsWidth == config.shift_stage_output_bitwidth)

  val delta = current_val.head.getBitsWidth - config.dout_bitwidth
  if (delta != 0) {
    val rounded =
      (if (delta > 0)
         Vec(
           current_val.zipWithIndex.map(t => {
             val x = t._1
             val i = t._2
             val x_shifted = x(delta, config.dout_bitwidth bits).setName("round_shifted_" + i)
             val x_negative = x.msb.setName("round_negative_" + i)

             val x_num_part = x_shifted(0, config.dout_bitwidth - 1 bits).setName("round_num_part_" + i)
             val x_saturated =
               Mux(x_negative, x_num_part.orR === False, x_num_part.andR === True).setName("round_saturated_" + i)

             val x_carry1 = x(delta - 1).setName("round_carry1_" + i)
             val x_rest_any = (if (delta >= 2) x(0, delta - 1 bits).orR else False).setName("round_rest_any_" + i)
             val x_delta =
               Mux(!x_saturated && x_carry1 && ((!x_negative) || x_rest_any), S(1), S(0)).setName("round_delta_" + i)
             x_shifted + x_delta
           })
         )
       else
         Vec(
           current_val.map(_.resize(config.dout_bitwidth))
         ))
    rounded.setName("rounded")
    val reg_rounded =
      RegNextWhen(rounded, controller.en(current_stage_count), init = rounded.getZero).setName("reg_rounded")
    current_val = reg_rounded
    current_stage_count += 1
  }

  assert(current_val.head.getBitsWidth == config.dout_bitwidth)

  dout.payload := current_val
}
