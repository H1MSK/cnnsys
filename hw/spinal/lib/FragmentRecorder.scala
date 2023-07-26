package lib

import spinal.core.Component.push
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/**
 *
 * @param depthBit max depth of target component
 *
 *  cur |   S1   |   S2   |   S3
 * -----|--------|--------|--------
 *   |ii|S1 == =0|S2 == ==|S3 == ==
 * oi|if|S1 +1 =0|S2 == +1|-- -- --
 *   |il|S2 +1 =0|S3 == +1|-- -- --
 * --|--|--------|--------|--------
 *   |ii|S1 -1 =0|S2 -1 ==|S3 -1 ==
 * of|if|S1 == =0|S2 -1 +1|-- -- --
 *   |il|S2 == =0|S3 -1 +1|-- -- --
 * --|--|--------|--------|--------
 *   |ii|-- -- --|S1 up =0|S2 up =0
 * ol|if|-- -- --|S1 u+ =0|S2 up =1
 *   |il|-- -- --|S2 u+ =0|S3 up =1
 *
 * ii: input idle
 * if: input fired
 * il: input fired and is last
 * similar for oi, of and ol, where o stands for output
 *
 * Content in body means [Next State] [Operation for first_fragment_rest_length] [Operation for second_*]
 *
 * Different operations:
 * `--`: This condition can not be satisfied
 * `=0`: set to 0
 * `=1`: set to 1
 * `+1`: increment by 1
 * `-1`: decrement by 1
 * `up`: assign from second_*
 * `u+`: assign from 1 + second_*
 */
case class FragmentRecorder[DIN_T <: Data, DOUT_T <: Data](
    din_type: HardType[DIN_T],
    dout_type: HardType[DOUT_T],
    depthBit: Int = 8
) extends Component {
  object State extends SpinalEnum {
    /**
     * S1: one fragment is being processed, i.e. input and output belongs to the same fragment
     * S2: two fragments are being processed
     * S3: two fragments are being processed, and the third fragment is halted at input
     */
    val S1, S2, S3 = newElement()
  }
  val din_frags = slave Stream Fragment(din_type())
  val din_stream = master Stream din_type()
  val dout_stream = slave Stream dout_type()
  val dout_frags = master Stream Fragment(dout_type())

  val first_fragment_rest_length = RegInit(U(0, depthBit bits))
  val second_fragment_rest_length = RegInit(U(0, depthBit bits))

  val state = RegInit(State.S1)
  dout_frags.payload.last := (first_fragment_rest_length === U(1, depthBit bits) && state =/= State.S1)

  val output_fire = dout_stream.valid && dout_frags.ready
  val output_last = first_fragment_rest_length === U(1)
  val din_halt = (state === State.S3 && !(output_last && output_fire))
  val input_fire = din_stream.ready && din_frags.valid && !din_halt
  val input_last = din_frags.last

  din_frags.ready := !din_halt && din_stream.ready
  din_stream.valid := !din_halt && din_frags.valid
  din_stream.payload := din_frags.payload.fragment

  dout_frags.arbitrationFrom(dout_stream)
  dout_frags.payload.fragment := dout_stream.payload

  when (state === State.S1) {
    when (input_fire && input_last) {
      state := State.S2
    }
    first_fragment_rest_length := first_fragment_rest_length + U(input_fire) - U(output_fire)
  }.elsewhen(state === State.S2) {
    when (output_fire && output_last) {
      first_fragment_rest_length := second_fragment_rest_length + U(input_fire)
      second_fragment_rest_length := 0
      when (!input_fire || !input_last) {
        state := State.S1
      }
    }.otherwise {
      first_fragment_rest_length := first_fragment_rest_length - U(output_fire)
      second_fragment_rest_length := second_fragment_rest_length + U(input_fire)
      when (input_fire && input_last) {
        state := State.S3
      }
    }
  }.otherwise {  // state == S3
    when (output_fire && output_last) {
      first_fragment_rest_length := second_fragment_rest_length
      second_fragment_rest_length := U(input_fire, depthBit bits)
      when (!input_fire || !input_last) {
        state := State.S2
      }
    }.elsewhen(output_fire && !output_last) {
      first_fragment_rest_length := first_fragment_rest_length - U(output_fire)
    }
  }
}
