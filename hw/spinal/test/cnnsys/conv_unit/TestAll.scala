package test.cnnsys.conv_unit

import test.TestTaskGenerator

object TestAll extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    TestConvCoreOutputTrimmer.prepare(included)
    TestConvUnit.prepare(included)
  }
}
