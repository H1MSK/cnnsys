package cnnsys.conv_unit

import lib.utils.TreeReduce
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ConvAddTree(input_bit_width: Int, length: Int, register_distance: Int = 2, extend_bitwidth: Boolean = true) extends Component {
  val din = slave Stream Vec(SInt(input_bit_width bits), length)
  val dout = master Stream SInt()

  val bias = in SInt(input_bit_width bits)

  def add(a: SInt, b: SInt) = if(extend_bitwidth) a +^ b else a + b

  private val tree = TreeReduce.withRegs(din.payload.toSeq :+ bias, register_distance, add)

  tree.controller << din
  tree.controller >> dout

  dout.payload := tree.result
}
