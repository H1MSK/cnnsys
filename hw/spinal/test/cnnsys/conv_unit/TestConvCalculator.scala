package test.cnnsys.conv_unit

import cnnsys.conv_unit.{ConvCalculator, ConvUnitConfig}
import spinal.core._
import spinal.core.sim._
import test.{TestTask, TestTaskGenerator}

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

object TestConvCalculator extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    val config = ConvUnitConfig()
    val task = new TestTask[ConvCalculator] {
      override def construct(): ConvCalculator = {
        val dut = ConvCalculator(config)
        dut.products.simPublic()
        dut.reg_products.simPublic()
        dut
      }

      override def doSim(compiled: SimCompiled[ConvCalculator]): Unit = {
        compiled.doSim(dut => {
          dut.clockDomain.forkStimulus(period = 10)
          dut.clockDomain.assertReset()

          dut.din.valid #= true
          dut.dout.ready #= true

          var cur_in: Array[Int] =
            TestTask.randomSInt(config.unitInDataBitWidth bits, length = config.kernelSize * config.kernelSize)
          dut.din.payload.zip(cur_in).foreach(t => t._1 #= t._2)

          (0 until 128).foreach(iter_kernel => {
            val kernel = Array.fill(config.kernelSize * config.kernelSize)(
              TestTask.randomSInt(config.unitKernelDataBitWidth bits)
            )
            dut.kernel_in.zip(kernel).foreach(t => t._1 #= t._2)

            val products = mutable.Queue[Array[Int]]()

            var iter_input = 0
            while(iter_input < 128) {
              // dut.dout.ready #= Random.nextBoolean()
              // dut.din.valid #= Random.nextBoolean()

              dut.clockDomain.waitActiveEdge()
              sleep(1)

              if (dut.din.valid.toBoolean && dut.din.ready.toBoolean) {
                products.enqueue(cur_in.zip(kernel).map(t => t._1 * t._2))
                cur_in =
                  TestTask.randomSInt(config.unitInDataBitWidth bits, length = config.kernelSize * config.kernelSize)
                dut.din.payload.zip(cur_in).foreach(t => t._1 #= t._2)
              }

              if (dut.dout.valid.toBoolean && dut.dout.ready.toBoolean) {
                val that_products = products.dequeue()
                that_products.indices.foreach(i => {
                  if (that_products(i) != dut.dout.payload(i).toInt) {
                    dut.clockDomain.waitActiveEdge()
                    simFailure(f"Product answer does not match: want ${that_products(i)}, but get ${dut.dout.payload(i).toInt}b${dut.dout.payload(i).getBitsWidth}\n" +
                      f"When checking #$i products in iter_input #$iter_input, iter_kernel #$iter_kernel.\n" +
                      f"kernel=${kernel.mkString("(", ", ", ")")},\n" +
                      f"products=${that_products.mkString("(", ", ", ")")},\n" +
                      f"output=${dut.dout.payload.map(_.toInt).mkString("(", ", ", ")")}")
                  }
                })
                iter_input += 1
              }
            }
          })
        })
      }
    }

    if (!included) task.excludeFromAll()
  }
}
