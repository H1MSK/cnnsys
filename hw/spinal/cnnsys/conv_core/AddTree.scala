package cnnsys.conv_core

import lib.utils.TreeReduce
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class AddTree(input_bit_width: Int, length: Int, register_distance: Int = 2) extends Component {
  val din = slave Stream Vec(UInt(input_bit_width bits), length)
  val dout = master Stream UInt()

  private val tree = TreeReduce.withRegs(din.payload, register_distance, (a: UInt, b: UInt) => a +^ b)

  tree.controller << din
  tree.controller >> dout

  dout.payload := tree.result
}
