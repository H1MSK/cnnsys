package cnnsys

import scala.collection.mutable.ListBuffer
import cnnsys.config.UnitConfig
import spinal.core._

case class LineBuffer (dataWidth: Int, inputLineWidth: Int, kernelSize: Int) extends Component {
  if (inputLineWidth < kernelSize)
    SpinalError("Input width should not be smaller than kernel size")

  val en = in Bool()
  val shift_input = in UInt(dataWidth bits)
  val line_width = in UInt(log2Up(inputLineWidth) bits)

  val shift_output = out UInt(dataWidth bits)

  var last = shift_input
  var cur: UInt = null

  var array = ListBuffer[UInt]()

  (0 until inputLineWidth).foreach(i => {
    cur = RegNextWhen(last, en)
    array += cur
    last = cur
  })

  shift_output := Vec(array)(line_width)

  var outputs = Vec(array.slice(0, kernelSize))
  out(outputs)

  array.indices.foreach(i => {
    array(i).setName("reg_" + i)
  })
}
