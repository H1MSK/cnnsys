package test.cnnsys

import test.TestTaskGenerator

object TestAll extends TestTaskGenerator {
  def prepare(included: Boolean): Unit = {
    conv_unit.TestAll.prepare(included)
    matmul_unit.TestAll.prepare(included)
  }
}
