package test.lib

import cnnsys.conv_unit.ConvUnitConfig
import lib.Input2DPadder
import spinal.core._
import spinal.core.sim._
import test.{TestTask, TestTaskGenerator}

import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.util.Random

object TestInput2DPadder extends TestTaskGenerator {
  def padding[T: ClassTag](array: Array[Array[T]], padding_data: T, padding_size: Int): Array[Array[T]] = {
    val width = array.head.length
    val tb_padding = Array.fill[Array[T]](padding_size)(Array.fill[T](width + 2 * padding_size)(padding_data))
    val lr_padding = Array.fill[T](padding_size)(padding_data)
    tb_padding ++ array.map(lr_padding ++ _ ++ lr_padding) ++ tb_padding
  }
  override def prepare(included: Boolean): Unit = {
    (0 until 4).foreach(max_padding_size => {
      Array(Array(2), (1 until 4).toArray, ConvUnitConfig.default.supportedInputWidths)
        .foreach(input_widths => {
          new TestTask[Input2DPadder[SInt]](included) {
            val config = new ConvUnitConfig() {
              supportedInputWidths = input_widths
            }

            override def construct(): Input2DPadder[SInt] = Input2DPadder(
              data_type = SInt(8 bits),
              supported_input_widths = input_widths,
              max_padding_size = max_padding_size
            )

            override def doSim(compiled: SimCompiled[Input2DPadder[SInt]]): Unit = {
              compiled.doSim(dut => {
                dut.clockDomain.forkStimulus(10)
                dut.clockDomain.assertReset()

                (0 to max_padding_size).foreach(padding_size => {
                  if (max_padding_size <= 0) {
                    assert(
                      dut.padding_size == null,
                      "Port padding_size should not be valid when max_padding_size <= 0"
                    )
                  } else {
                    dut.padding_size #= padding_size
                  }

                  config.supportedInputWidths.indices.foreach(sel => {

                    val padding_data = TestTask.randomSInt(config.coreInDataBitWidth bits)

                    val width = config.supportedInputWidths(sel)
                    val inputs = Array.fill(10)(
                      Array.fill(width)(
                        TestTask.randomSInt(8 bits)
                      )
                    )
                    val outputs =
                      padding(inputs, padding_data, padding_size)

                    dut.padding_data #= padding_data
                    if (max_padding_size != 0)
                      dut.padding_size #= padding_size
                    if (input_widths.length > 1)
                      dut.line_width_sel #= sel

                    dut.din.valid #= false
                    dut.dout.ready #= false
                    dut.din.fragment #= 0
                    dut.din.last #= false
                    dut.clockDomain.waitActiveEdge()

                    def testWith(the_inputs: Array[Int], the_outputs: Array[Int]): Unit = {
                      var in_ptr = 0
                      var out_ptr = 0
                      var next_input = true

                      while (out_ptr != the_outputs.length) {
                        sleep(1)
                        dut.dout.ready #= Random.nextBoolean()
                        dut.din.valid #= (in_ptr < the_inputs.length || !next_input) && Random.nextBoolean()

                        if (next_input) {
                          if (in_ptr < the_inputs.length) {
                            next_input = false
                            dut.din.fragment #= the_inputs(in_ptr)
                            in_ptr += 1
                            dut.din.last #= in_ptr == the_inputs.length
                          } else {
                            dut.din.fragment #= 0
                            dut.din.last #= false
                          }
                        }

                        dut.clockDomain.waitActiveEdge()

                        if (dut.din.ready.toBoolean && dut.din.valid.toBoolean) {
                          next_input = true
                        }
                        if (dut.dout.ready.toBoolean && dut.dout.valid.toBoolean) {
                          val out = dut.dout.fragment.toInt
                          val std = the_outputs(out_ptr)
                          val last = dut.dout.last.toBoolean

                          if (the_outputs(out_ptr) != out) {
                            sleep(1)
                            simFailure(
                              s"Unexpected dout data. Want ${std}, but get ${out}\n" +
                                s"width = $width, max_padding = $max_padding_size, padding = $padding_size, paddata = $padding_data, in_ptr = $in_ptr, out_ptr = $out_ptr\n" +
                                s"inputs = \n${the_inputs.sliding(width, width).map(_.mkString(",")).mkString("\n")}\n" +
                                s"standard = \n${the_outputs.sliding(width+2*padding_size, width+2*padding_size).map(_.mkString(",")).mkString("\n")}"
                            )
                          }
                          out_ptr += 1

                          val expected_last = (out_ptr == the_outputs.length)
                          if (last != expected_last) {
                            sleep(1)
                            simFailure(
                              s"Unexpected last signal. Want $expected_last, but get $last\n" +
                                s"width = $width, max_padding = $max_padding_size, padding = $padding_size, paddata = $padding_data, in_ptr = $in_ptr, out_ptr = $out_ptr\n" +
                                s"inputs = \n${the_inputs.sliding(width, width).map(_.mkString(",")).mkString("\n")}\n" +
                                s"standard = \n${the_outputs.sliding(width + 2 * padding_size, width + 2 * padding_size).map(_.mkString(",")).mkString("\n")}"
                            )
                          }
                        }
                      }
                    }

                    testWith(inputs.flatten, outputs.flatten)

                    val cut_length = Random.nextInt(width)
                    val modified_inputs = inputs
                      .slice(0, inputs.length - 1)
                      .appended(inputs.last.slice(0, inputs.last.length - cut_length))
                    val modified_outputs = padding(
                      modified_inputs,
                      padding_data,
                      padding_size
                    )

                    testWith(modified_inputs.flatten, modified_outputs.flatten)
                  })
                })

              })
            }
          }
        })
    })
  }

}
