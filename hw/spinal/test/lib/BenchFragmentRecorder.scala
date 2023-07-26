package test.lib

import _root_.lib.FragmentRecorder
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class BenchFragmentRecorder() extends Component {
  val din_frags = slave Stream Fragment(UInt(8 bits))
  val din_stream = Stream(UInt(8 bits))
  val dout_frags = master Stream Fragment(UInt(8 bits))

  var current: Stream[UInt] = din_stream


  val recorder = FragmentRecorder(UInt(8 bits), UInt(8 bits), 4)
  recorder.din_frags << din_frags
  recorder.din_stream >> din_stream

  for (_ <- 0 until 4) {
    val next = Stream(UInt(8 bits))
    next <-/< current
    current = next
  }

  recorder.dout_stream << current
  recorder.dout_frags >> dout_frags
}
