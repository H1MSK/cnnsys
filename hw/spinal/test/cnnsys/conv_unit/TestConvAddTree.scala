package test.cnnsys.conv_unit
import spinal.core.sim.{SimCompiled, _}
import cnnsys.conv_unit.ConvAddTree

import scala.collection.mutable
import scala.util.Random
import spinal.core._
import spinal.lib._
import test.{TestTask, TestTaskGenerator}

object TestConvAddTree extends TestTaskGenerator {
  private def testFor(bw: Int, length: Int, dist: Int, extend: Boolean, included: Boolean = true): Unit = {
    new TestTask[ConvAddTree](included) {
      override def construct(): ConvAddTree =
        ConvAddTree(
          input_bit_width = bw,
          length = length,
          register_distance = dist,
          extend_bitwidth = extend,
          use_bias = true
        )

      override def doSim(compiled: SimCompiled[ConvAddTree]): Unit = {
        def setInput(dut: ConvAddTree) = {
          val cur = Array.fill(length + 1)(Random.nextInt(1 << bw) - (1 << (bw - 1)))

          for (i <- dut.din.payload.indices) {
            dut.din.payload(i) #= cur(i)
          }
          dut.bias #= cur.last
          cur
        }

        compiled.doSim(s"Functional")(dut => {
          dut.clockDomain.forkStimulus(period = 10)
          dut.clockDomain.assertReset()

          val sums = new mutable.Queue[Int]()
          val inputs = new mutable.Queue[Array[Int]]()
          var successCount = 0

          var cur: Array[Int] = setInput(dut)
          while (successCount < 300) {
            dut.dout.ready #= Random.nextBoolean()
            dut.din.valid #= Random.nextBoolean()

            dut.clockDomain.waitActiveEdge()

            if (dut.din.ready.toBoolean && dut.din.valid.toBoolean) {
              sums.enqueue(cur.reduce((a, b) => {
                val sum = a + b
                if (extend) sum
                else if (sum >= (1 << (bw - 1))) {
                  sum - (1 << bw)
                } else if (sum < -(1 << (bw - 1))) {
                  sum + (1 << bw)
                } else {
                  sum
                }
              }))
              inputs.enqueue(cur)
              cur = setInput(dut)
            }

            if (dut.dout.valid.toBoolean && dut.dout.ready.toBoolean) {
              val sum = sums.dequeue()
              val in = inputs.dequeue()

              if (dut.dout.payload.toInt != sum)
                simFailure(
                  s"dout(${dut.dout.payload.toInt}b${dut.dout.payload.getBitsWidth}) != sum($sum). Inputs = ${in
                      .mkString("(", ", ", ")")}, bitwidth = $bw"
                )

              successCount += 1
            }

          }

          simSuccess()
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

  override def prepare(included: Boolean): Unit = {
    for (bw <- Array(4, 8, 12, 16)) {
      for (length <- Array(1, 2, 4, 6, 8, 16)) {
        for (dist <- 1 until 4) {
          for (extend <- Array(false, true)) testFor(bw, length, dist, extend, included = included)
        }
      }
    }
  }
}
