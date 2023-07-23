package test

import _root_.cnnsys.ProjectConfig
import spinal.core._
import spinal.core.sim._
import test.TestTask.runningTasks

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.util.Random

abstract class TestTaskBase {
  def runnable: Runnable
}

abstract class TestTask[T <: Component] extends TestTaskBase {
  def construct(): T

  def doSim(compiled: SimCompiled[T]): Unit

  final def runnable: Runnable = () => {
    try {
      TestTask.compilingTasks.getAndAdd(1)
      var compiled: SimCompiled[T] = null
      try {
        compiled = ProjectConfig.sim.compile(construct())
      } finally {
        TestTask.compilingTasks.getAndAdd(-1)
      }
      TestTask.compiledTasks.getAndAdd(1)
      TestTask.runningTasks.getAndAdd(1)
      try {
        doSim(compiled)
      } finally {
        TestTask.runningTasks.getAndAdd(-1)
      }
    } catch {
      case e: Throwable => TestTask.throwException(e)
    } finally {
      TestTask.finishedTasks.getAndAdd(1)
    }
  }

  final def excludeFromAll(): Unit = TestTask.tasks -= this

  TestTask.tasks += this
}

object TestTask {
  private var executer: Option[ExecutorService] = None
  private val tasks = mutable.Set[TestTaskBase]()

  val compilingTasks: AtomicInteger = new AtomicInteger(0)
  val compiledTasks: AtomicInteger = new AtomicInteger(0)
  val runningTasks: AtomicInteger = new AtomicInteger(0)
  val finishedTasks: AtomicInteger = new AtomicInteger(0)

  var exception: Option[Throwable] = None

  def prepareAll(): Unit = {
    cnnsys.TestTask.prepare()
  }

  def throwException(e: Throwable): Unit = {
    this.exception = Option(e)
  }

  def runAll(numOfThreads: Int): Unit = {
    if (executer.isDefined) {
      executer.get.shutdown()
      executer = None
    }

    executer = Option(Executors.newFixedThreadPool(numOfThreads))

    tasks.foreach(t => executer.get.execute(t.runnable))

    try {
      while (tasks.size != finishedTasks.get() && exception.isEmpty) {
        printState()
        Thread.sleep(20)
      }

      println(f"Simulation ${if (exception.isDefined) "failed" else "succeeded"}.")
      if (exception.isDefined) {
        throw exception.get
      }
    } finally {
      executer.get.shutdown()
    }
  }

  def printState(): Unit = {
    print(f"Compiling: ${compilingTasks.get()}%-3d Running: ${runningTasks.get()}%-3d Compiled: ${compiledTasks
        .get()}%-5d Finished:${finishedTasks.get()}%-5d/${tasks.size}%d\r")
  }

  def main(args: Array[String]): Unit = {
    prepareAll()
    runAll(Runtime.getRuntime.availableProcessors())
  }

  def randomUInt(bitCount: BitCount): Int = Random.nextInt(1 << bitCount.value)
  def randomSInt(bitCount: BitCount): Int = Random.nextInt(1 << bitCount.value) - (1 << (bitCount.value - 1))

  def randomUInt(bitCount: BitCount, length: Int): Array[Int] = Array.fill(length)(randomUInt(bitCount))
  def randomSInt(bitCount: BitCount, length: Int): Array[Int] = Array.fill(length)(randomSInt(bitCount))
}
