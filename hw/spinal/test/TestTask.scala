package test

import _root_.cnnsys.ProjectConfig
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

abstract class TestTask[T <: Component](included: Boolean) extends TestTaskBase {
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

  if (included)
    TestTask.tasks += this
}

object TestTask {
  val tasks: mutable.Set[TestTaskBase] = mutable.Set[TestTaskBase]()

  def randomUInt(bitCount: BitCount): Int = {
    assert(bitCount.value <= 31)
    Random.nextInt(1 << bitCount.value)
  }
  def randomUIntAsLong(bitCount: BitCount): Long = {
    assert(bitCount.value <= 63)
    Random.nextLong(1L << bitCount.value)
  }

  def randomSInt(bitCount: BitCount): Int = {
    assert(bitCount.value <= 31)
    Random.nextInt(1 << bitCount.value) - (1 << (bitCount.value - 1))
  }

  def randomSIntAsLong(bitCount: BitCount): Long = {
    assert(bitCount.value <= 63)
    Random.nextLong(1L << bitCount.value) - (1L << (bitCount.value - 1))
  }

  def randomUInt(bitCount: BitCount, length: Int): Array[Int] = Array.fill(length)(randomUInt(bitCount))
  def randomSInt(bitCount: BitCount, length: Int): Array[Int] = Array.fill(length)(randomSInt(bitCount))

  def inRangeOfSInt(bitCount: BitCount, x: Int): Boolean = {
    assert(bitCount.value < 32)
    val upper = 1 << (bitCount.value - 1)
    x >= -upper && x < upper
  }

  def inRangeOfSInt(bitCount: BitCount, x: Long): Boolean = {
    assert(bitCount.value < 64)
    val upper = 1L << (bitCount.value - 1)
    x >= -upper && x < upper
  }

  def inRangeOfUInt(bitCount: BitCount, x: Int): Boolean = {
    assert(bitCount.value < 32)
    x >= 0 && x < (1 << bitCount.value)
  }

  def inRangeOfUInt(bitCount: BitCount, x: Long): Boolean = {
    assert(bitCount.value < 64)
    x >= 0 && x < (1L << bitCount.value)
  }
}
