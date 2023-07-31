package test.cnnsys.conv_unit

import test.TestTaskGenerator

object TestAll extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    TestConvCalculator.prepare(included)
    TestConvCoreOutputTrimmer.prepare(included)
    TestConvKernelMem.prepare(included)
    TestConvUnit.prepare(included)
  }
}
