package test.cnnsys.matmul_unit

import test.TestTaskGenerator

object TestAll extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = TestMatMulUnit.prepare(included)
}
