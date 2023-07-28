package test

object TestAll extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    cnnsys.TestAll.prepare(included)
    lib.TestAll.prepare(included)
  }
}
