package cnnsys.conv_core

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class KernelMem(mem_size: Int, data_width: Int) extends Component {
  val din = slave Flow UInt(data_width bits)
  val dout = master Flow UInt(data_width bits)

  val regs = Vec(History(RegNext(din.payload, din.payload.getZero), mem_size, din.valid, din.payload.getZero).reverse)
  regs.foreach(out(_))

  dout.valid := din.valid
  dout.payload := regs.head
}
