package test.lib

import test.TestTaskGenerator


object TestAll extends TestTaskGenerator {
  def prepare(included: Boolean): Unit = {
    requantizer.TestAll.prepare(included)
    StreamController.TestAll.prepare(included)
    TestAddTree.prepare(included)
    TestFragmentRecorder.prepare(included)
    TestInput2DPadder.prepare(included)
    TestVectorOperator.prepare(included)
    TestWindowedHistory2D.prepare(included)
  }
}
