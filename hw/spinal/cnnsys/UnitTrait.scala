package cnnsys

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}

trait UnitTrait {
  def genStreamPort(bitCount: BitCount, useLast: Boolean = false): Stream[Axi4Stream.Axi4StreamBundle] = Axi4Stream(
    Axi4StreamConfig((bitCount.value + 7) / 8, useLast = useLast)
  )

  def connectStreams[T1 <: Data, T2 <: Data](from: Stream[T1], to: Stream[T2]): Unit = {
    to.arbitrationFrom(from)
    to.payload.assignFromBits(from.payload.asBits)
  }

  def connectStreamToFlow[T1 <: Data, T2 <: Data](from: Stream[T1], to: Flow[T2]): Unit = {
    to.valid := from.valid
    from.ready := True
    to.payload.assignFromBits(from.payload.asBits)
  }
}
