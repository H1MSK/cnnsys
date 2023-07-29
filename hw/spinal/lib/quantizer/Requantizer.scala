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

  private var last_payload = din.payload

  val stage_count = (if (config.useScale) 1 else 0) +
    (if (config.useOffset) 1 else 0) +
    (if (config.useRightShift) 1 else 0) +
    (if (config.shift_stage_output_bitwidth != config.dout_bitwidth) 1 else 0)

  val controller = StreamController(stage_count)
  controller << din
  var current_stage = 0

  if (config.useScale) {
    val scaled = Vec(last_payload.map(_ * param.scale))
    last_payload = RegNextWhen(scaled, controller.en(current_stage), init = scaled.getZero)
    current_stage += 1
    scaled.setName("scaled")
    last_payload.setName("scale_stage")
  }

  if (config.useOffset) {
    val offset = Vec(
      last_payload.map(x => if (config.useOffsetSaturation) x +| param.offset else x +^ param.offset)
    )
    last_payload = RegNextWhen(offset, controller.en(current_stage), init = offset.getZero)
    current_stage += 1
    offset.setName("offset")
    last_payload.setName("offset_stage")
  }

  if (config.useRightShift) {
    val shifted = Vec(last_payload.map(_ >> param.shift_count))
    last_payload = RegNextWhen(shifted, controller.en(current_stage), init = shifted.getZero)
    current_stage += 1
    shifted.setName("shifted")
    last_payload.setName("shift_stage")
  }

  assert(last_payload.head.getBitsWidth == config.shift_stage_output_bitwidth)

  val delta = last_payload.head.getBitsWidth - config.dout_bitwidth
  if (delta != 0) {
    val rounded =
      (if (delta > 0)
         Vec(
           last_payload.zipWithIndex.map(t => {
             val x = t._1
             val i = t._2
             val x_shifted = x(delta, config.dout_bitwidth bits).setName("round_shifted_" + i)
             val x_negative = x.msb.setName("round_negative_" + i)

             val x_num_part = x_shifted(0, config.dout_bitwidth - 2 bits).setName("round_num_part_" + i)
             val x_saturated = Mux(x_negative, x_num_part.orR === False, x_num_part.andR === True).setName("round_saturated_" + i)

             val x_carry1 = x(delta - 1).setName("round_carry1_" + i)
             val x_rest_any = (if (delta >= 2) x(0, delta - 1 bits).orR else False).setName("round_rest_any_" + i)
             val x_delta = Mux(!x_saturated && x_carry1 && ((!x_negative) || x_rest_any), S(1), S(0)).setName("round_delta_" + i)
             x_shifted + x_delta
           })
         )
       else
         Vec(
           last_payload.map(_.resize(config.dout_bitwidth))
         ))
    last_payload = RegNextWhen(rounded, controller.en(current_stage), init = rounded.getZero)
    current_stage += 1
    rounded.setName("rounded")
    last_payload.setName("round_stage")
  }

  controller >> dout
  dout.payload := last_payload
}
