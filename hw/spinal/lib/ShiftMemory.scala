package lib

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ShiftMemory[T <: Data](data_type: HardType[T], size: Int, enable_shift_out: Boolean) extends Component {
  val din = slave Flow data_type()
  val dout = enable_shift_out generate master Flow data_type()

  /** History of din,
    * If din is forwardly inputted,
    * regs will be **reversed**
    */
  val regs: Vec[T] = History(
    RegNextWhen(din.payload, din.valid, init = din.payload.getZero),
    size,
    din.valid,
    din.payload.getZero
  )
  regs.indices.foreach(i => {
    val r = regs(i)
    out(r)
    r.setName("history_" + i)
  })

  if (enable_shift_out) {
    dout.valid := din.valid
    dout.payload := regs.last
  }
}
