package test

import _root_.cnnsys.ProjectConfig
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

abstract class TestTask[T <: Component] extends TestTaskBase {
  def construct(): T

  def doSim(compiled: SimCompiled[T]): Unit

  final def toRunnable(generator: TestTaskGenerator): Runnable = () => {
    try {
      generator.compilingTasks.getAndAdd(1)
      var compiled: SimCompiled[T] = null
      try {
        compiled = ProjectConfig.sim.compile(construct())
      } finally {
        generator.compilingTasks.getAndAdd(-1)
      }
      generator.compiledTasks.getAndAdd(1)
      generator.runningTasks.getAndAdd(1)
      try {
        doSim(compiled)
      } finally {
        generator.runningTasks.getAndAdd(-1)
      }
    } catch {
      case e: Throwable => generator.throwException(e)
    } finally {
      generator.finishedTasks.getAndAdd(1)
    }
  }

  final def excludeFromAll(): Unit = TestTask.tasks -= this

  TestTask.tasks += this
}

object TestTask {
  val tasks: mutable.Set[TestTaskBase] = mutable.Set[TestTaskBase]()

  def randomUInt(bitCount: BitCount): Int = Random.nextInt(1 << bitCount.value)
  def randomSInt(bitCount: BitCount): Int = Random.nextInt(1 << bitCount.value) - (1 << (bitCount.value - 1))

  def randomUInt(bitCount: BitCount, length: Int): Array[Int] = Array.fill(length)(randomUInt(bitCount))
  def randomSInt(bitCount: BitCount, length: Int): Array[Int] = Array.fill(length)(randomSInt(bitCount))
}
