package cnnsys.conv_unit

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ConvKernelMem(mem_size: Int, data_width: Int) extends Component {
  val din = slave Flow SInt(data_width bits)
  val dout = master Flow SInt(data_width bits)

  val regs: Vec[SInt] = Vec(History(RegNextWhen(din.payload, din.valid, init = din.payload.getZero), mem_size, din.valid, din.payload.getZero).reverse)
  regs.indices.foreach(i => {
    val r = regs(i)
    out(r)
    r.setName("kernel_" + i)
  })

  dout.valid := din.valid
  dout.payload := regs.head
}
