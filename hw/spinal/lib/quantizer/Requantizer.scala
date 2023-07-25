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

  val stage_count = (if (config.scale_bitwidth > 0) 1 else 0) +
    (if (config.useOffset) 1 else 0) +
    (if (config.useRightShift) 1 else 0) +
    (if (
       config.din_bitwidth +
         Math.max(0, config.scale_bitwidth) +
         (if (config.useOffset && !config.useOffsetSaturation) 1 else 0) -
         config.dout_bitwidth != 0
     ) 1
     else 0)

  val controller = StreamController(stage_count)
  controller << din
  var current_stage = 0

  if (config.scale_bitwidth > 0) {
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

  if (last_payload.head.getBitsWidth - config.dout_bitwidth != 0) {
    val rounded =
      (if (last_payload.head.getBitsWidth - config.dout_bitwidth > 0)
         Vec(
           last_payload.map(_.roundToInf(last_payload.head.getBitsWidth - config.dout_bitwidth bits, align = false).sat(1))
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
