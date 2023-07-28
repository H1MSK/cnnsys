package lib

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class WindowedHistory[T <: Data](
    data_type: HardType[T],
    supported_input_widths: Array[Int],
    visible_input_count: Int,
    has_shift_output: Boolean
) extends Component {
  if (!(supported_input_widths.sorted sameElements supported_input_widths)) {
    SpinalError("Elements in supported_input_widths should be in ascent order")
  }

  val shift_in = slave Flow data_type()

  val line_width_sel =
    (has_shift_output && supported_input_widths.length > 1) generate Bits(log2Up(supported_input_widths.length) bits)

  val shift_out = has_shift_output generate data_type()

  private val history = History[T](
    RegNextWhen(shift_in.payload, shift_in.valid, init = shift_in.payload.getZero),
    if (has_shift_output) supported_input_widths.last else visible_input_count,
    when = shift_in.valid,
    init = shift_in.payload.getZero
  )
  history.indices.foreach(i => history(i).setName("history_" + i))

  /**
   * The **last** input is at exports.head
   */
  val exports = Vec(history.slice(0, visible_input_count))
  out(exports)

  if (has_shift_output) {
    out(shift_out)

    if (supported_input_widths.length > 1) {
      in(line_width_sel)

      val muxList = supported_input_widths.indices
        .map(i => i -> history(supported_input_widths(i) - 1))
      shift_out := line_width_sel.muxList(
        if (isPow2(muxList.length)) muxList
        else muxList.appended(default -> history(supported_input_widths.last - 1))
      )
    } else {
      shift_out := history(supported_input_widths.head - 1)
    }
  }
}
