package test.cnnsys.conv_unit

import test.TestTaskGenerator

object TestAll extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    TestConvAddTree.prepare(included)
    TestConvCalculator.prepare(included)
    TestConvKernelMem.prepare(included)
  }
}
