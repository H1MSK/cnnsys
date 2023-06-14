package cnnsys

import spinal.core._
import spinal.lib.History

case class KernelMem(mem_size: Int, data_width: Int) extends Component {
  val shift_en = in Bool()
  val shift_in = in UInt(data_width bits)

  val regs = History(shift_in, mem_size, shift_en)
  regs.foreach(p => out(p))
}
