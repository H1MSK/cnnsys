package test.lib

import lib.AddTree
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import test.{TestTask, TestTaskGenerator}

import scala.language.postfixOps
import scala.util.Random

object TestAddTree extends TestTaskGenerator {
  override def threadCount: Int = 1
  private def testFor(
      bw: Int,
      length: Int,
      dist: Int,
      extend: Boolean,
      saturate: Boolean,
      included: Boolean = true
  ): Unit = {
    new TestTask[AddTree](included) {
      override def construct(): AddTree =
        AddTree(
          input_bit_width = bw,
          length = length,
          register_distance = dist,
          extend_bitwidth = extend,
          saturate_output = saturate,
          use_bias = true
        )

      override def doSim(compiled: SimCompiled[AddTree]): Unit = {
        def setInput(dut: AddTree, input: Array[Int]): Unit = {
          assert(dut.length + (if (dut.use_bias) 1 else 0) == input.length)
          for (i <- dut.din.payload.indices) {
            dut.din.payload(i) #= input(i)
          }
          dut.bias #= input.last
        }

        compiled.doSim(s"Functional")(dut => {
          dut.clockDomain.forkStimulus(period = 10)
          dut.clockDomain.assertReset()

          val inputs = Array.fill(300)(
            TestTask.randomSInt(bw bits, length + 1)
          )
          val sums = inputs.map(x => addUp(x, extend = extend, saturate = saturate, bw = bw))

          var in_ptr = 0
          var out_ptr = 0
          var next_input = true
          while (out_ptr < sums.length) {
            sleep(1)
            dut.dout.ready #= Random.nextBoolean()
            dut.din.valid #= (in_ptr < inputs.length || !next_input) && Random.nextBoolean()

            if (next_input) {
              if (in_ptr < inputs.length) {
                next_input = false
                setInput(dut, inputs(in_ptr))
                in_ptr += 1
              } else {
                setInput(dut, Array.fill(length + 1)(0))
              }
            }

            dut.clockDomain.waitActiveEdge()

            if (dut.din.ready.toBoolean && dut.din.valid.toBoolean) {
              next_input = true
            }

            if (dut.dout.valid.toBoolean && dut.dout.ready.toBoolean) {
              val std = sums(out_ptr)
              val out = dut.dout.payload.toInt
              if (out != std) {
                sleep(1)
                simFailure(
                  s"Unexpected out, want $std, but get $out\n" +
                    s"Params: in_ptr=$in_ptr, out_ptr=$out_ptr, bw=$bw, length=$length, dist=$dist, extend=$extend, saturate=$saturate"
                )
              }

              out_ptr += 1
            }

          }
        })

        // Under current implementation, LatencyAnalysis will use field algoIncrementale,
        // which is checked(==-1) in operator #=. So latency analysis should be put the last
        val dut = compiled.dut
        val hardware_latency = LatencyAnalysis(dut.din.valid, dut.dout.valid)
        // Total count of addends are length + 1(bias)
        val calculated_latency = (log2Up(length + 1) + dist - 1) / dist
        if (calculated_latency != hardware_latency)
          simFailure(
            s"Calculated latency($calculated_latency) != hardware latency($hardware_latency). length = $length, dist = $dist"
          )
      }
    }
  }

  def addUp(cur: Array[Int], extend: Boolean, saturate: Boolean, bw: Int) = {
    val out = cur.reduce((a, b) => {
      val sum = a + b
      if (extend) sum
      else if (sum >= (1 << (bw - 1))) {
        sum - (1 << bw)
      } else if (sum < -(1 << (bw - 1))) {
        sum + (1 << bw)
      } else {
        sum
      }
    })
    if (saturate && !TestTask.inRangeOfSInt(bw bits, out)) {
      val topbit = 1 << (bw - 1)
      if (out > 0) topbit - 1
      else -topbit
    } else out
  }

  override def prepare(included: Boolean): Unit = {
    for (bw <- Array(4, 8, 12, 16)) {
      for (length <- Array(1, 2, 4, 6, 8, 16)) {
        for (dist <- 1 until 4) {
          for (extend <- Array(false, true))
            for (saturate <- Array(false, true)) testFor(bw, length, dist, extend, saturate, included = included)
        }
      }
    }
  }
}
