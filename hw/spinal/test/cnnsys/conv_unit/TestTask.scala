package test.cnnsys.conv_unit

object TestTask {
  def prepare(): Unit = {
    TestConvAddTree.prepare(false)
    TestConvCalculator.prepare(false)
    TestConvKernelMem.prepare(true)
  }
}
