package test.cnnsys.conv_unit

import cnnsys.conv_unit.{ConvKernelMem, ConvUnitConfig}
import spinal.core._
import spinal.core.sim._
import test.{TestTask, TestTaskGenerator}

import scala.collection.mutable
import scala.language.postfixOps

object TestConvKernelMem extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    val config = ConvUnitConfig.default
    new TestTask[ConvKernelMem] {
      override def construct(): ConvKernelMem =
        ConvKernelMem(config.kernelSize * config.kernelSize, config.unitKernelDataBitWidth)

      override def doSim(compiled: SimCompiled[ConvKernelMem]): Unit = {
        compiled.doSim(dut => {
          dut.clockDomain.forkStimulus(10)
          dut.clockDomain.assertReset()
          (0 until 1024).foreach(_ => {
            val kernel =
              TestTask.randomSInt(config.unitKernelDataBitWidth bits, length = config.kernelSize * config.kernelSize)

            var last_kernel: Array[Int] = null
            val out_data = mutable.Queue[Int]()

            kernel.foreach(data => {
              dut.din.valid #= true
              dut.din.payload #= data
              dut.clockDomain.waitActiveEdge()
              out_data.enqueue(dut.dout.payload.toInt)
              sleep(1)
            })

            dut.regs.indices.foreach(i => {
              if (dut.regs(i).toInt != kernel(i)) {
                simFailure(s"Kernel #${i} is ${dut.regs(i).toInt}, but want ${kernel(i)}\n" +
                  s"kernel = ${kernel.mkString(",")}\n" +
                  s"regs = ${dut.regs.map(_.toInt).mkString(",")}")
              }
            })

            if (last_kernel != null) {
              last_kernel.indices.foreach(i => {
                val out = out_data(i)
                if (last_kernel(i) != out) {
                  simFailure(s"Kernel out #${i} is ${last_kernel(i)}, but want ${out}\n" +
                    s"generated = ${last_kernel.mkString(",")}\n" +
                    s"regs = ${dut.regs.map(_.toInt).mkString(",")}")
                }
              })
            }
            kernel.indices.foreach(_ => out_data.dequeue())

            last_kernel = kernel
          })
        })
      }
    }
  }
}
