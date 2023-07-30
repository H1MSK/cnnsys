package cnnsys.conv_unit

import lib.utils.TreeReduce
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ConvAddTree(
    input_bit_width: Int,
    length: Int,
    register_distance: Int,
    extend_bitwidth: Boolean,
    saturate_output: Boolean,
    use_bias: Boolean
) extends Component {
  val din = slave Stream Vec(SInt(input_bit_width bits), length)
  val dout = master Stream SInt()

  val bias = use_bias generate SInt(input_bit_width bits)
  if (use_bias) in(bias)

  def add(a: SInt, b: SInt) = if (extend_bitwidth) a +^ b else a + b

  private val tree =
    TreeReduce.withRegs(
      if (use_bias) {
        din.payload.toSeq :+ bias
      } else {
        din.payload.toSeq
      },
      register_distance,
      add
    )

  tree.controller << din
  tree.controller >> dout

  if (saturate_output) {
    val saturated = tree.result.sat(tree.result.getBitsWidth - input_bit_width).setName("saturated")
    dout.payload := saturated
  } else {
    dout.payload := tree.result
  }

}
