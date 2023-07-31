package test.lib.requantizer

import test.TestTaskGenerator

object TestAll extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = TestRequantizerChain.prepare(included)
}
