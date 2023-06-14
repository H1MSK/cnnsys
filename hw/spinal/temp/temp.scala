package temp

import spinal.core._

case class ComponentT1() extends Component {
  val b = in Bool()
  val o = out UInt(8 bits)
  val i = in UInt(8 bits)

  o.setAsReg() init(0)

  when(b) {
    o := o + i
  }
}

case class temp() extends Component{
  val en = in Bool()

  val x0 = UInt(8 bits)

  val o = out UInt(8 bits)

  x0 := U(1)

  val arr = Array.fill(5)(ComponentT1())

  var last = x0

  arr.foreach(c => {
    c.i := last
    c.b := en
    last = c.o
  })

  o := last
}

object TempTest extends App {
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "."
  ).generate(new temp()).printPruned()
}
