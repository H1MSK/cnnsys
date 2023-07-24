package test.lib.StreamController

import test.TestTaskGenerator

object TestAll extends TestTaskGenerator {
  def prepare(included: Boolean): Unit = TestStreamController.prepare(included)
}