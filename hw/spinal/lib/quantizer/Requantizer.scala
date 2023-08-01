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

  if (config.useScale) {
    val scaled = Vec(current_val.map(_ * param.scale))
    current_val = scaled
    scaled.setName("scaled")
  }

  if (config.useOffset) {
    val offset = Vec(
      current_val.map(x => if (config.useOffsetSaturation) x +| param.offset else x +^ param.offset)
    )
    current_val = offset
    offset.setName("offset")
  }

  if (config.useRightShift) {
    val shifted = Vec(current_val.map(_ >> param.shift_count))
    current_val = shifted
    shifted.setName("shifted")
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
             val x_saturated = Mux(x_negative, x_num_part.orR === False, x_num_part.andR === True).setName("round_saturated_" + i)

             val x_carry1 = x(delta - 1).setName("round_carry1_" + i)
             val x_rest_any = (if (delta >= 2) x(0, delta - 1 bits).orR else False).setName("round_rest_any_" + i)
             val x_delta = Mux(!x_saturated && x_carry1 && ((!x_negative) || x_rest_any), S(1), S(0)).setName("round_delta_" + i)
             x_shifted + x_delta
           })
         )
       else
         Vec(
           current_val.map(_.resize(config.dout_bitwidth))
         ))
    current_val = rounded
    rounded.setName("rounded")
  }

  dout <-/< din.translateWith(current_val)
}
