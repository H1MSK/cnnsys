package test.cnnsys.conv_unit

import cnnsys.conv_unit.{ConvCoreOutputTrimmer, ConvUnitConfig}
import spinal.core._
import spinal.core.sim._
import test.{TestTask, TestTaskGenerator}

import scala.language.postfixOps
import scala.util.Random

object TestConvCoreOutputTrimmer extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    (1 until 4).foreach(kernel_size => {
      Array(Array(11), (0 until 4).map(kernel_size + _).toArray, ConvUnitConfig.default.supportedInputWidths).foreach(input_widths => {
        new TestTask[ConvCoreOutputTrimmer](included) {
          val config = new ConvUnitConfig() {
            kernelSize = kernel_size
            supportedInputWidths = input_widths
          }
          override def construct(): ConvCoreOutputTrimmer = ConvCoreOutputTrimmer(config)

          override def doSim(compiled: SimCompiled[ConvCoreOutputTrimmer]): Unit = {
            compiled.doSim(dut => {
              dut.clockDomain.forkStimulus(10)
              dut.clockDomain.assertReset()

              config.supportedInputWidths.indices.foreach(sel => {
                val width = config.supportedInputWidths(sel)
                val inputs = Array.fill(10)(
                  Array.fill(width)(
                    TestTask.randomSInt(config.coreOutDataBitWidth bits, config.coreOutChannelCount)
                  )
                )
                val outputs = inputs
                  .slice(config.kernelSize - 1, inputs.length)
                  .map(line => {
                    line.slice(config.kernelSize - 1, line.length)
                  })

                val flat_inputs = inputs.flatten
                val flat_outputs = outputs.flatten

                if (dut.line_width_sel != null)
                  dut.line_width_sel #= sel

                var in_ptr = 0
                var out_ptr = 0
                var next_input = true
                while (out_ptr != flat_outputs.length) {
                  dut.dout.ready #= Random.nextBoolean()
                  dut.din.valid #= (in_ptr < flat_inputs.length || !next_input) && Random.nextBoolean()

                  if (next_input) {
                    if (in_ptr < flat_inputs.length) {
                      next_input = false
                      dut.din.fragment.zip(flat_inputs(in_ptr)).foreach(t => t._1 #= t._2)
                      in_ptr += 1
                      dut.din.last #= in_ptr == flat_inputs.length
                    } else {
                      dut.din.fragment.foreach(_ #= 0)
                      dut.din.last #= false
                    }
                  }

                  dut.clockDomain.waitActiveEdge()

                  if (dut.din.ready.toBoolean && dut.din.valid.toBoolean) {
                    next_input = true
                  }
                  if (dut.dout.ready.toBoolean && dut.dout.valid.toBoolean) {
                    val out = dut.dout.fragment.map(_.toInt).toArray
                    val std = flat_outputs(out_ptr)
                    val last = dut.dout.last.toBoolean

                    if (!flat_outputs(out_ptr).sameElements(out)) {
                      simFailure(
                        s"Unexpected dout data. Want ${std.mkString("(", ", ", ")")}, but get ${out.mkString("(", ", ", ")")}\n" +
                          s"kernel_size = $kernel_size, width = $width, in_ptr = $in_ptr, out_ptr = $out_ptr"
                      )
                    }
                    out_ptr += 1

                    val expected_last = (out_ptr == flat_outputs.length)
                    if (last != expected_last) {
                      simFailure(s"Unexpected last signal. Want $expected_last, but get $last")
                    }
                  }
                }
              })
            })
          }
        }
      })
    })
  }

}
