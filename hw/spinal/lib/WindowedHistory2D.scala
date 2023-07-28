package lib

import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.language.postfixOps

/** 2D window for input flow, for conv, polling, etc.
  * @param line_count Number of line
  * @param data_type Element type
  * @param supported_input_widths Options of input widths, selected directly by `line_width_sel` scalar port (**no register**)
  * @param visible_input_count How many data on each line is exported
  * @tparam T Data type
  */
case class WindowedHistory2D[T <: Data](
    line_count: Int,
    data_type: HardType[T],
    supported_input_widths: Array[Int],
    visible_input_count: Int
) extends Component {

  /** Data input port
    */
  val shift_in = slave Flow data_type()

  /** Select port for line width
    */
  val line_width_sel =
    (line_count > 1 && supported_input_widths.length > 1) generate Bits(log2Up(supported_input_widths.length) bits)

  private var previous_shift_payload = shift_in.payload

  private var lines = (0 until line_count).map(i => WindowedHistory(
    data_type = data_type,
    supported_input_widths = supported_input_widths,
    visible_input_count = visible_input_count,
    has_shift_output = i + 1 != line_count
  ))

  if (line_width_sel != null) {
    in(line_width_sel)
  }

  lines.foreach(line => {
    line.shift_in.valid := shift_in.valid
    line.shift_in.payload := previous_shift_payload
    if (line.line_width_sel != null)
      line.line_width_sel := line_width_sel
    previous_shift_payload = line.shift_out
  })

  lines.indices.foreach(i => lines(i).setName("line_" + i))

  /** Windowed data, **reversed**, in **row-major** order
    */
  val window = out Vec (data_type(), line_count * visible_input_count)
  window.zip(lines.flatMap(_.exports)).foreach(t => t._1 := t._2)
}
