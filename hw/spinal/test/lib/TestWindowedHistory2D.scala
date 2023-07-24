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
      for (supported_input_widths <- Array(Array(6), Array(3, 6, 9, 12)))
        for (visible_input_count <- Array(1, 3)) {

          val task = new TestTask[WindowedHistory2D[SInt]] {
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

                Random.shuffle(supported_input_widths.indices.toList).foreach(sel => {
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
                      if (!excepted)
                        simFailure(
                          "Unnecessary port generated: " +
                            "Port line_width_sel is generated when condition \"supported_input_widths.length > 1 && line_count > 1\" doesn't match" +
                            s"Config:\n" +
                            s"  line_count = $line_count,\n" +
                            s"  supported_input_widths = ${supported_input_widths.mkString(", ")},\n" +
                            s"  visible_input_count = $visible_input_count\n"
                        )
                    }

                    val data = TestTask.randomSInt(8 bits, (line_count + 2) * current_width)
                    for (l <- 0 until (line_count + 2)) {
                      for (c <- 0 until current_width) {
                        val v = data(l * current_width + c)
                        dut.shift_in.payload #= v
                        dut.shift_in.valid #= true
                        dut.clockDomain.waitActiveEdge()
                        if (l >= line_count - 1 && c >= visible_input_count - 1) {
                          var i = 0
                          for (check_l <- (l - line_count + 1 to l))
                            for (check_c <- (c - visible_input_count + 1 to c)) {
                              val d = data(check_l * current_width + check_c)
                              val out = dut.window(i).toInt
                              if (out != d) {
                                simFailure(
                                  s"Window data doesn't match, want $d, but get $out\n" +
                                    s"Checking pos (r=${check_l}, c=${check_c}), current input at (r=$l, c=$c)\n" +
                                    s"Config:\n" +
                                    s"  line_count = $line_count,\n" +
                                    s"  supported_input_widths = ${supported_input_widths.mkString(", ")},\n" +
                                    s"  visible_input_count = $visible_input_count\n" +
                                    s"Data:\n${data.sliding(current_width).map(_.mkString(", ")).mkString("\n")}\n"
                                )
                              }
                            }
                        }
                      }
                    }
                  })
              })
            }
          }

          if (!included) task.excludeFromAll()
        }
  }
}
