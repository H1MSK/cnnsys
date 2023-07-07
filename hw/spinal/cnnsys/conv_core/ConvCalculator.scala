package cnnsys.conv_core

import lib.StreamController.StreamController
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ConvCalculator(config: ConvUnitConfig) extends Component {
  val din = slave Stream Vec(UInt(config.coreInDataBitWidth bits), config.kernelSize * config.kernelSize)

  val kernel_in = in Vec(
    UInt(config.coreKernelDataBitWidth bits),
    config.kernelSize * config.kernelSize
  )

  val dout = master Stream UInt(config.singleChannelSumDataBitWidth bits)

  private val product_controller = StreamController(1)

  product_controller << din

  private val products = Vec(kernel_in.indices.map(i => din.payload(i) * kernel_in(i)))
  private val reg_products = RegNextWhen(products, product_controller.en(0))

  private val addTree = AddTree(products.head.getBitsWidth, products.length)

  product_controller >> addTree.din
  addTree.din.payload := reg_products

  addTree.dout >> dout
}
