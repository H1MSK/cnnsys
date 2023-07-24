package test

abstract class TestTaskBase {
  def toRunnable(generator: TestTaskGenerator): Runnable
}
