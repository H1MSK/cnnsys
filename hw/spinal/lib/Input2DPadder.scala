package lib

import cnnsys.conv_unit.ConvUnitConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

import scala.language.postfixOps

case class Input2DPadder[T <: Data](data_type: HardType[T], supported_input_widths: Array[Int], max_padding_size: Int)
    extends Component {
  if (!(supported_input_widths.sorted sameElements supported_input_widths)) {
    SpinalError("Elements in supported_input_widths should be in ascent order")
  }

  val din = slave Stream Fragment(data_type())
  val dout = master Stream Fragment(data_type())

  val padding_data = in(data_type())
  val padding_size = (max_padding_size > 0) generate UInt(log2Up(max_padding_size + 1) bits)

  val line_width_sel =
    (supported_input_widths.length > 1) generate Bits(log2Up(supported_input_widths.length) bits)

  if (supported_input_widths.length > 1) in(line_width_sel)

  if (max_padding_size == 0) {
    dout << din
  } else {
    in(padding_size)
    val muxList = supported_input_widths.indices.map(i => i -> U(supported_input_widths(i)))
    val width =
      (if (supported_input_widths.length > 1)
         line_width_sel.muxList(
           if (isPow2(muxList.length)) muxList
           else muxList.appended(default -> U(supported_input_widths.last))
         )
       else
         U(supported_input_widths.last)).setName("width")

    val counter =
      RegInit(U(0, (supported_input_widths.last + 2 * max_padding_size) * max_padding_size bits)).setName("counter")
    val counterPlusOne = (counter + U(1)).setName("counterPlusOne")

    val zero = counter.getZero.setName("zero")

    val dout_fired = dout.fire.setName("dout_fired")

    din.ready := False
    dout.valid := False

    dout.fragment := padding_data
    dout.last := False

    val padding_size_by_2 = (padding_size * 2).setName("padding_size_x2")

    // comment on "- padding size":
    // The top and bottom paddings will both output line *
    val side_padding_count = ((width + 2 * padding_size) * padding_size - padding_size).setName("side_padding_size")

    val finishing = RegInit(False).setName("finishing")

    /**
     * state-transition diagram in DOT:
     * {{{
     * digraph StateTrans {
     *   idle -> start_end [label="din.valid"]
     *   start_end -> run_pad [label="!finish"]
     *   start_end -> idle [label="finish"]
     *   run_pad -> run [label="!finish"]
     *   run_pad -> start_end[label="finish"]
     *   run -> run_pad
     *
     *   idle[tooltip="reset finish"]
     *   run[tooltip="set finish when din.last asserted"]
     * }
     * }}}
     *
     * For input 3x3 and padding 2, each point is output by state:
     * {{{
     * ss sss ss
     * ss sss pp
     * pp ddd pp
     * pp ddd pp
     * pp ddd pp
     * pp eee ee
     * ee eee ee
     * }}}
     *
     * For input 3x3+2:
     * {{{
     * ss sss ss
     * ss sss pp
     * pp ddd pp
     * pp ddd pp
     * pp ddd pp
     * pp ddP pp
     * pe eee ee
     * ee eee E
     * }}}
     */
    val fsm = new StateMachine {
      // Wait till din.valid == 1
      val idle = new State with EntryPoint
      // Enter from idle(din.valid == 1), output top padding
      val starting_ending_padding = new State
      // Output padding * 2
      val running_padding = new State
      // Output inputs
      val running_data = new State
      // Enter from running(din.last == 1), output right padding of current line and bottom padding

      idle
        .whenIsActive {
          finishing := False
          when(din.valid) {
            when (padding_size === U(0)) {
              goto(running_data)
            }.otherwise {
              goto(starting_ending_padding)
            }
          }
        }

      starting_ending_padding
        .onEntry {
          counter := zero
        }
        .whenIsActive {
          din.ready := False
          dout.valid := True

          when(dout_fired) {
            when(counterPlusOne >= side_padding_count) {
              when(finishing) {
                dout.last := True
                goto(idle)
              }.otherwise {
                goto(running_padding)
              }
            }.otherwise {
              counter := counterPlusOne
            }
          }
        }

      running_padding
        .onEntry {
          counter := zero
        }
        .whenIsActive {
          din.ready := False
          dout.valid := True

          when(dout_fired) {
            when(counterPlusOne >= padding_size_by_2) {
              when(finishing) {
                goto(starting_ending_padding)
              }.otherwise {
                goto(running_data)
              }
            }.otherwise {
              counter := counterPlusOne
            }
          }
        }

      running_data
        .onEntry {
          counter := 0
        }
        .whenIsActive {
          din.ready := dout.ready
          dout.valid := din.valid
          dout.fragment := din.fragment
          when(padding_size === U(0)) {
            dout.last := din.last
          }
          when(dout_fired) {
            when(din.last) {
              finishing := True
              when(padding_size === U(0)) {
                goto(idle)
              }.otherwise {
                goto(running_padding)
              }
            }.elsewhen(counterPlusOne >= width) {
              when(padding_size === U(0)) {
                counter := 0
              }.otherwise {
                goto(running_padding)
              }
            }.otherwise {
              counter := counterPlusOne
            }
          }
        }
    }.setName("fsm")
  }
}
