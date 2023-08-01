package test.lib

import cnnsys.conv_unit.ConvUnitConfig
import lib.VectorOperator
import spinal.core._
import spinal.core.sim._
import test.{TestTask, TestTaskGenerator}

import scala.language.postfixOps
import scala.util.Random

object TestVectorOperator extends TestTaskGenerator {
  def testFor(
      in_bits: Int,
      static_bits: Int,
      out_bits: Int,
      length: Int,
      operation: (SInt, SInt) => SInt,
      soft_operation: (Int, Int) => Int,
      included: Boolean
  ): Unit = {

    new TestTask[VectorOperator[SInt, SInt, SInt]](included) {
      override def construct(): VectorOperator[SInt, SInt, SInt] = {
        val dut = VectorOperator(
          SInt(in_bits bits),
          SInt(static_bits bits),
          SInt(out_bits bits),
          length = length,
          operation = operation
        )
        dut.outputs.simPublic()
        dut.reg_outputs.simPublic()
        dut
      }

      override def doSim(compiled: SimCompiled[VectorOperator[SInt, SInt, SInt]]): Unit = {
        compiled.doSim(dut => {
          dut.clockDomain.forkStimulus(period = 10)
          dut.clockDomain.assertReset()

          dut.din.valid #= true
          dut.dout.ready #= true

          (0 until 128).foreach(iter_static => {
            val static_in = Array.fill(length)(
              TestTask.randomSInt(static_bits bits)
            )
            dut.static_in.zip(static_in).foreach(t => t._1 #= t._2)

            val inputs = Array.fill(128)(
              TestTask.randomSInt(in_bits bits, length = length)
            )

            val outputs =
              inputs.map(_.zip(static_in).map(t => soft_operation(t._1, t._2)))

            var in_ptr = 0
            var out_ptr = 0

            var next_input = true

            while (out_ptr < outputs.length) {
              dut.dout.ready #= Random.nextBoolean()
              dut.din.valid #= (in_ptr < outputs.length || !next_input) && Random.nextBoolean()

              if (next_input) {
                if (in_ptr < outputs.length) {
                  next_input = false
                  val cur_in = inputs(in_ptr)
                  in_ptr += 1
                  dut.din.payload.zip(cur_in).foreach(t => t._1 #= t._2)
                } else {
                  dut.din.payload.foreach(_ #= 0)
                }
              }

              dut.clockDomain.waitActiveEdge()

              if (dut.din.valid.toBoolean && dut.din.ready.toBoolean) {
                next_input = true
              }

              if (dut.dout.valid.toBoolean && dut.dout.ready.toBoolean) {
                val that_products = outputs(out_ptr)
                that_products.indices.foreach(i => {
                  if (that_products(i) != dut.dout.payload(i).toInt) {
                    dut.clockDomain.waitActiveEdge()
                    simFailure(
                      s"Output does not match: want ${that_products(i)}, " +
                        s"but get ${dut.dout.payload(i).toInt}b${dut.dout.payload(i).getBitsWidth}\n" +
                        s"iter_static=$iter_static, in_ptr=$in_ptr, out_ptr=$out_ptr.\n" +
                        s"in=${inputs(out_ptr).mkString("(", ", ", ")")},\n" +
                        s"static_in=${static_in.mkString("(", ", ", ")")},\n" +
                        s"standard=${that_products.mkString("(", ", ", ")")},\n" +
                        s"output=${dut.dout.payload.map(_.toInt).mkString("(", ", ", ")")}"
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

  override def prepare(included: Boolean): Unit = {
    (7 to 9).foreach(in_width =>
      (7 to 9).foreach(static_width =>
        Array(8, 9, 16).foreach(length => {
          testFor(
            in_width,
            static_width,
            out_bits = in_width + static_width,
            length = length,
            operation = (a: SInt, b: SInt) => a * b,
            soft_operation = (a: Int, b: Int) => a * b,
            included = included
          )
          testFor(
            in_width,
            static_width,
            out_bits = Math.max(in_width, static_width) + 1,
            length = length,
            operation = (a: SInt, b: SInt) => a +^ b,
            soft_operation = (a: Int, b: Int) => a + b,
            included = included
          )
        })
      )
    )
  }
}
