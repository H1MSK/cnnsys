package lib

import lib.StreamController.StreamController
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class AddTree(
    input_bit_width: Int,
    length: Int,
    register_distance: Int,
    extend_bitwidth: Boolean,
    saturate_output: Boolean,
    use_bias: Boolean,
    enable_recurse: Boolean = true
) extends Component {
  val din = slave Stream Vec(SInt(input_bit_width bits), length)
  val dout = master Stream SInt()

  val bias = use_bias generate SInt(input_bit_width bits)
  if (use_bias) in(bias)

  def add(a: SInt, b: SInt): SInt = if (extend_bitwidth) a +^ b else a + b

  var inputs = din
  if (use_bias) {
    inputs = din.translateWith(Vec(din.payload.appended(bias)))
  }

  val max_inputs_in_one_stage = 1 << register_distance
  var level_count = 0
  if (inputs.payload.length > max_inputs_in_one_stage) {
    while (inputs.payload.length != 1) {
      assert(enable_recurse)
      val group_count = (inputs.payload.length + max_inputs_in_one_stage - 1) / max_inputs_in_one_stage
      val outputs = (0 until group_count)
        .map(ith => {
          val ith_group_inputs =
            inputs.payload
              .slice((ith * inputs.payload.length) / group_count, ((ith + 1) * inputs.payload.length) / group_count)
          val ith_add_tree = AddTree(
            input_bit_width = ith_group_inputs.head.getBitsWidth,
            length = ith_group_inputs.length,
            register_distance = register_distance,
            extend_bitwidth = extend_bitwidth,
            // Only use saturate on the output
            saturate_output = false,
            use_bias = false,
            enable_recurse = false
          ).setName(s"ConvAddTree_${level_count}_$ith")

          ith_add_tree.din.payload := Vec(ith_group_inputs)
          ith_add_tree.din.valid := inputs.valid
          if (ith == 0) {
            inputs.ready := ith_add_tree.din.ready
          }
          ith_add_tree.dout
        })
        .toArray

      val bitwidths = outputs.map(_.payload.getBitsWidth)
      val max_output_bitwidth = bitwidths.max
      assert(bitwidths.min + 1 >= max_output_bitwidth)
      val out_payload = Vec(outputs.map(_.payload.resize(max_output_bitwidth bits)))

      inputs = new Stream(out_payload).setName(s"level_${level_count}")
      inputs << outputs.head.translateWith(out_payload)
      outputs.slice(1, outputs.length).foreach(_.ready := inputs.ready)

      level_count += 1
    }

    var result = inputs.payload.head

    if (saturate_output) {
      val saturated = result.sat(result.getBitsWidth - input_bit_width)
      result = saturated
    }

    dout << inputs.translateWith(result)

  } else {
    var result = inputs.payload.reduceBalancedTree(add).setName("reduce_result")
    if (extend_bitwidth && saturate_output) {
      val saturated = result.sat(result.getBitsWidth - input_bit_width).setName("saturated")
      result = saturated
    }

    val controller = StreamController(1)
    controller << inputs
    val reg_result = RegNextWhen(result, controller.en(0), init = result.getZero)
    controller >> dout
    dout.payload := reg_result
  }
}
