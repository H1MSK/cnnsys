package lib.quantizer.blackboxes

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}


/**
 *  Result = A * B
 */
case class FP32Multiplier(useLast: Boolean = false) extends BlackBox {
  val aclk = in Bool ()
  val S_AXIS_A = slave(Axi4Stream(Axi4StreamConfig(dataWidth = 4, useLast = useLast)))
  val S_AXIS_B = slave(Axi4Stream(Axi4StreamConfig(dataWidth = 4)))
  val M_AXIS_RESULT = master(Axi4Stream(Axi4StreamConfig(dataWidth = 4, useLast = useLast)))

  mapClockDomain(clock = aclk)
}
