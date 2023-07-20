package lib.quantizer

import blackboxes._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.Axi4Stream

import scala.language.postfixOps

object QuantizerType extends Enumeration {
  val Symmetric, Asymmetric = Value
}

/** Perform dout = round_fn(din * recip_scale [+ offset] )
  */
case class FP32Quantizer(out_bitwidth: Int, parallel_count: Int, quantizerType: QuantizerType.Value) {
  val din = slave Stream Vec(SInt(32 bits), parallel_count)
  val dout = master Stream Vec(SInt(out_bitwidth bits), parallel_count)

  val recip_scale = in Bits (32 bits)
  val offset = (quantizerType == QuantizerType.Asymmetric) generate in Bits (32 bits)

  if (quantizerType == QuantizerType.Symmetric) {
    val quantizer = Array.fill(parallel_count)(FP32Multiplier())
    (0 until parallel_count).foreach(i => {
      val q = quantizer(i)
      q.S_AXIS_A.arbitrationFrom(din)
    })
  }
}
