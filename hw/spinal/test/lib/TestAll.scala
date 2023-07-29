package test.lib

import test.TestTaskGenerator


object TestAll extends TestTaskGenerator {
  def prepare(included: Boolean): Unit = {
    StreamController.TestAll.prepare(included)
    TestWindowedHistory2D.prepare(included)
    TestFragmentRecorder.prepare(included)
    TestInput2DPadder.prepare(included)
  }
}
