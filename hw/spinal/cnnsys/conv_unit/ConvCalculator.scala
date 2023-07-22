package cnnsys.conv_unit

import lib.StreamController.StreamController
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ConvCalculator(config: ConvUnitConfig) extends Component {
  val din = slave Stream Vec(SInt(config.unitInDataBitWidth bits), config.kernelSize * config.kernelSize)

  val kernel_in = in Vec(
    SInt(config.unitKernelDataBitWidth bits),
    config.kernelSize * config.kernelSize
  )

  val dout = master Stream Vec(SInt(config.productDataBitWidth bits), config.kernelSize * config.kernelSize)

  val product_controller = StreamController(1)

  product_controller << din

  val products = Vec(din.payload.zip(kernel_in).map(t => t._1 * t._2))
  val reg_products = RegNextWhen(products, product_controller.en(0), init = products.getZero)

  product_controller >> dout
  dout.payload := reg_products
}
