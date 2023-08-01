package lib

import lib.StreamController.StreamController
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class VectorOperator[T_DIN <: Data, T_KERNEL <: Data, T_DOUT <: Data](
    din_type: HardType[T_DIN],
    kernel_type: HardType[T_KERNEL],
    dout_type: HardType[T_DOUT],
    length: Int,
    operation: (T_DIN, T_KERNEL) => T_DOUT
) extends Component {
  val din = slave Stream Vec(din_type(), length)

  val static_in = in Vec (
    kernel_type(),
    length
  )

  val dout = master Stream Vec(dout_type(), length)

  val operation_controller = StreamController(1)

  operation_controller << din

  val outputs = Vec(din.payload.zip(static_in).map(t => operation(t._1, t._2)))
  val reg_outputs = RegNextWhen(outputs, operation_controller.en(0), init = outputs.getZero)

  operation_controller >> dout
  dout.payload := reg_outputs
}
