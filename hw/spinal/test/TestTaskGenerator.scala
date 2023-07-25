package test

import test.TestTask.tasks

import java.util.concurrent.{ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicInteger

abstract class TestTaskGenerator {
  def prepare(included: Boolean): Unit

  private var executer: Option[ExecutorService] = None
  val compilingTasks: AtomicInteger = new AtomicInteger(0)
  val compiledTasks: AtomicInteger = new AtomicInteger(0)
  val runningTasks: AtomicInteger = new AtomicInteger(0)
  val finishedTasks: AtomicInteger = new AtomicInteger(0)

  var exception: Option[Throwable] = None

  def throwException(e: Throwable): Unit = {
    this.exception = Option(e)
  }

  def runAll(numOfThreads: Int): Unit = {
    if (executer.isDefined) {
      executer.get.shutdown()
      executer = None
    }

    executer = Option(Executors.newFixedThreadPool(numOfThreads))

    tasks.foreach(t => {
      executer.get.execute(t.toRunnable(this))
    })

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
      executer.get.shutdownNow()
    }
  }

  def printState(): Unit = {
    print(f"Compiling: ${compilingTasks.get()}%-3d Running: ${runningTasks.get()}%-3d Compiled: ${
      compiledTasks
        .get()
    }%-5d Finished:${finishedTasks.get()}%-5d/${tasks.size}%d\r")
  }

  def main(args: Array[String]): Unit = {
    prepare(true)
    runAll(Runtime.getRuntime.availableProcessors())
  }
}