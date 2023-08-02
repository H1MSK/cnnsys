package lib.StreamController

import spinal.core._
import spinal.lib.{History, Stream}

case class StreamController(depth: Int, collapse_bubble: Boolean = true) extends Component {
  assert(depth > 0)

  val ivalid = in Bool()
  val iready = out Bool()
  val ovalid = out Bool()
  val oready = in Bool()
  val en = out Vec(Bool(), depth)
  val full_bit = (depth == 1) generate Bool()

  if (depth == 1) {
    full_bit.setAsReg() init false
    iready := !full_bit || oready
    ovalid := full_bit
    full_bit := ivalid || (full_bit && !oready)
    en(0) := ivalid && (!full_bit || oready)
  } else {
    if (collapse_bubble) {
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
    } else {
      val can_shift = Bool().setName("can_shift")
      val valids = History(ivalid, depth + 1, when = can_shift, init = False).setName("rValid")
      can_shift := (valids(depth) === False || oready)
      ovalid := valids(depth)
      iready := can_shift
      en.foreach(_ := can_shift)
    }
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
