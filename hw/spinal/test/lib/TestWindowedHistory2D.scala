package test.lib

import lib.WindowedHistory2D
import spinal.core._
import spinal.core.sim.{SimCompiled, _}
import test.{TestTask, TestTaskGenerator}

import scala.language.postfixOps
import scala.util.Random

object TestWindowedHistory2D extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    for (line_count <- Array(1, 3))
      for (supported_input_widths <- Array(Array(6), Array(6, 9, 12), Array(3, 6, 9, 12)))
        for (visible_input_count <- Array(1, 3)) {

          new TestTask[WindowedHistory2D[SInt]](included) {
            override def construct(): WindowedHistory2D[SInt] = {
              WindowedHistory2D(
                line_count = line_count,
                data_type = SInt(8 bits),
                supported_input_widths = supported_input_widths,
                visible_input_count = visible_input_count
              )
            }

            override def doSim(compiled: SimCompiled[WindowedHistory2D[SInt]]): Unit = {
              compiled.doSim(dut => {
                dut.clockDomain.forkStimulus(period = 10)
                dut.clockDomain.assertReset()

                dut.shift_in.valid #= false

                Random
                  .shuffle(supported_input_widths.indices.toList)
                  .foreach(sel => {
                    val current_width = supported_input_widths(sel)
                    if (supported_input_widths.length > 1 && line_count > 1)
                      dut.line_width_sel #= sel
                    else {
                      var excepted = false
                      try {
                        dut.line_width_sel #= sel
                      } catch {
                        case e: NullPointerException => excepted = true
                      }
                      if (!excepted) {
                        sleep(1)
                        simFailure(
                          "Unnecessary port generated: " +
                            "Port line_width_sel is generated when condition \"supported_input_widths.length > 1 && line_count > 1\" doesn't match" +
                            s"Config:\n" +
                            s"  line_count = $line_count,\n" +
                            s"  supported_input_widths = ${supported_input_widths.mkString(", ")},\n" +
                            s"  visible_input_count = $visible_input_count\n"
                        )
                      }
                    }

                    val data = TestTask.randomSInt(8 bits, (line_count + 2) * current_width)
                    val data_lines = data.sliding(line_count, line_count)

                    var ptr = 0

                    while (ptr < data.length) {
                      sleep(1)
                      val v = data(ptr)
                      dut.shift_in.payload #= v
                      val valid = Random.nextBoolean()
                      dut.shift_in.valid #= valid

                      dut.clockDomain.waitActiveEdge()
                      sleep(1)

                      if (valid) {
                        val l = ptr / current_width
                        val c = ptr % current_width
                        ptr += 1
                        if (l >= line_count - 1 && c >= visible_input_count - 1) {
                          val out = dut.window.map(_.toInt).toArray
                          val std = (l - line_count + 1 to l).flatMap(cl => {
                            (c - visible_input_count + 1 to c).map(cc => data(cl * current_width + cc))
                          }).reverse.toArray
                          if (!(out sameElements std)) {
                            simFailure(
                              s"Window data doesn't match, want ${std.mkString("(", ", ", ")")}, but get ${out.mkString("(", ", ", ")")}\n" +
                                s"Config:\n" +
                                s"  line_count = $line_count,\n" +
                                s"  supported_input_widths = ${supported_input_widths.mkString(", ")},\n" +
                                s"  current_input_width = $current_width,\n" +
                                s"  visible_input_count = $visible_input_count\n" +
                                s"Data:\n${data.sliding(current_width, current_width).map(_.mkString(", ")).mkString("\n")}\n"
                            )
                          }
                        }
                      }
                    }
                  })
              })
            }
          }
        }
  }
}
