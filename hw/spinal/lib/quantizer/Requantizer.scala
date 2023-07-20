package lib.quantizer

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

  private val din_stage = din.stage()

  private var last_stage = din_stage

  if (config.scale_bitwidth > 0) {
    val scale_stage = Stream(Vec(SInt(config.din_bitwidth + config.scale_bitwidth bits), parallel_count))
    scale_stage.arbitrationFrom(last_stage)
    scale_stage.payload := Vec(last_stage.payload.map(_ * param.scale))
    scale_stage.setName("scale_stage")
    last_stage = scale_stage.stage()
  }

  if (config.useOffset) {
    val offset_stage = Stream(
      Vec(SInt(config.din_bitwidth + config.scale_bitwidth + (if (config.useOffsetSaturation) 0 else 1) bits), parallel_count)
    )
    offset_stage.arbitrationFrom(last_stage)
    offset_stage.payload := Vec(
      last_stage.payload.map(x => if (config.useOffsetSaturation) x +^ param.offset else x +| param.offset)
    )
    offset_stage.setName("offset_stage")
    last_stage = offset_stage.stage()
  }

  if (config.useRightShift) {
    val shift_stage = Stream(Vec(SInt(last_stage.payload.head.getBitsWidth bits), parallel_count))
    shift_stage.arbitrationFrom(last_stage)
    shift_stage.payload := Vec(last_stage.payload.map(_ >> param.shift_count))
    shift_stage.setName("shift_stage")
    last_stage = shift_stage.stage()
  }

  val round_stage = Stream(Vec(SInt(config.dout_bitwidth bits), parallel_count))
  round_stage.arbitrationFrom(last_stage)
  round_stage.payload := Vec(
    last_stage.payload.map(_.roundToInf(last_stage.payload.head.getBitsWidth - config.dout_bitwidth bits, align = true))
  )
  last_stage = round_stage

  dout.arbitrationFrom(last_stage)
  dout.payload := last_stage.payload
}
