package test.lib

import cnnsys.conv_unit.ConvUnitConfig
import lib.ShiftMemory
import spinal.core._
import spinal.core.sim._
import test.{TestTask, TestTaskGenerator}

import scala.collection.mutable
import scala.language.postfixOps

object TestShiftMemory extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    new TestTask[ShiftMemory[SInt]](included) {
      override def construct(): ShiftMemory[SInt] =
        ShiftMemory(
          data_type = SInt(8 bits),
          size = 16,
          enable_shift_out = true
        )

      override def doSim(compiled: SimCompiled[ShiftMemory[SInt]]): Unit = {
        compiled.doSim(dut => {
          dut.clockDomain.forkStimulus(10)
          dut.clockDomain.assertReset()
          (0 until 1024).foreach(_ => {
            val data =
              TestTask.randomSInt(8 bits, length = 16)

            var last_data: Array[Int] = null
            val out_data = mutable.Queue[Int]()

            data.foreach(data => {
              dut.din.valid #= true
              dut.din.payload #= data
              dut.clockDomain.waitActiveEdge()
              out_data.enqueue(dut.dout.payload.toInt)
              sleep(1)
            })

            val stored = dut.regs.map(_.toInt).reverse

            stored.indices.foreach(i => {
              if (stored(i) != data(i)) {
                simFailure(
                  s"Stored #${i} is ${stored(i)}, but want ${data(i)}\n" +
                    s"data = ${data.mkString(",")}\n" +
                    s"regs = ${stored.mkString(",")}"
                )
              }
            })

            if (last_data != null) {
              last_data.indices.foreach(i => {
                val out = out_data(15 - i)
                if (last_data(i) != out) {
                  simFailure(
                    s"Kernel out #${i} is ${last_data(i)}, but want ${out}\n" +
                      s"generated = ${last_data.mkString(",")}\n" +
                      s"regs = ${dut.regs.map(_.toInt).mkString(",")}"
                  )
                }
              })
            }
            data.indices.foreach(_ => out_data.dequeue())

            last_data = data
          })
        })
      }
    }
  }
}
