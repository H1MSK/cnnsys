package lib.StreamController

import spinal.core._
import spinal.lib.Stream

case class StreamController(depth: Int) extends Component {
  assert(depth > 0)

  val ivalid = in Bool()
  val iready = out Bool()
  val ovalid = out Bool()
  val oready = in Bool()
  val en = out Vec(Bool(), depth)

  if (depth == 1) {
    val full_bit = Reg(Bool()) init false
    full_bit.setName("full_bit")

    iready := !full_bit || oready
    ovalid := full_bit
    full_bit := ivalid || (full_bit && !oready)
    en(0) := ivalid && (!full_bit || oready)

  } else {
    val controllers = Array.fill(depth)(StreamController(1))
    controllers.indices.foreach(i => controllers(i).setName("Controller_" + i))
    val leftController = controllers.head
    val rightController = controllers.last

    (0 until controllers.length - 1).foreach(i => {
      controllers(i) >> controllers(i + 1)
    })

    leftController.ivalid := ivalid
    iready := leftController.iready
    ovalid := rightController.ovalid
    rightController.oready := oready
    en := Vec(controllers.flatMap(_.en))
  }

  def >>(that: StreamController) = {
    this.oready := that.iready
    that.ivalid := this.ovalid
  }

  def <<(that: StreamController) = {
    that >> this
  }

  def >>[T <: Data](that: Stream[T]) = {
    this.oready := that.ready
    that.valid := this.ovalid
  }

  def <<[T <: Data](that: Stream[T]) = {
    this.ivalid := that.valid
    that.ready := this.iready
  }
}
