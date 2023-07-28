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
    val config = ConvUnitConfig.default
    new TestTask[ConvCalculator](included) {
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

          (0 until 128).foreach(iter_kernel => {
            val kernel = Array.fill(config.kernelSize * config.kernelSize)(
              TestTask.randomSInt(config.unitKernelDataBitWidth bits)
            )
            dut.kernel_in.zip(kernel).foreach(t => t._1 #= t._2)

            val inputs = Array.fill(128)(
              TestTask.randomSInt(config.unitInDataBitWidth bits, length = config.kernelSize * config.kernelSize)
            )

            val products = inputs.map(_.zip(kernel).map(t => t._1 * t._2))

            var in_ptr = 0
            var out_ptr = 0

            var next_input = true

            while (out_ptr < products.length) {
              dut.dout.ready #= Random.nextBoolean()
              dut.din.valid #= (in_ptr < products.length || !next_input) && Random.nextBoolean()

              if (next_input) {
                if (in_ptr < products.length) {
                  next_input = false
                  val cur_in = inputs(in_ptr)
                  in_ptr += 1
                  dut.din.payload.zip(cur_in).foreach(t => t._1 #= t._2)
                } else {
                  // keep it true to indicate input finished
                  // next_input = true
                  dut.din.payload.foreach(_ #= 0)
                }
              }

              dut.clockDomain.waitActiveEdge()

              if (dut.din.valid.toBoolean && dut.din.ready.toBoolean) {
                next_input = true
              }

              if (dut.dout.valid.toBoolean && dut.dout.ready.toBoolean) {
                val that_products = products(out_ptr)
                that_products.indices.foreach(i => {
                  if (that_products(i) != dut.dout.payload(i).toInt) {
                    dut.clockDomain.waitActiveEdge()
                    simFailure(
                      f"Product answer does not match: want ${that_products(i)}, but get ${dut.dout.payload(i).toInt}b${dut.dout.payload(i).getBitsWidth}\n" +
                        f"iter_kernel=$iter_kernel, in_ptr=$in_ptr, out_ptr=$out_ptr.\n" +
                        f"kernel=${kernel.mkString("(", ", ", ")")},\n" +
                        f"products=${that_products.mkString("(", ", ", ")")},\n" +
                        f"output=${dut.dout.payload.map(_.toInt).mkString("(", ", ", ")")}"
                    )
                  }
                })
                out_ptr += 1
              }
            }
          })
        })
      }
    }
  }
}
