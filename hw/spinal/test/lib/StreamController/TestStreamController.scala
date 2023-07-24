package test.lib.StreamController

import spinal.core.sim._
import cnnsys.ProjectConfig
import lib.StreamController.StreamController
import test.{TestTask, TestTaskGenerator}

import scala.collection.mutable
import scala.util.Random

object TestStreamController extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    var task = new TestTask[StreamController] {
      override def construct(): StreamController = {
        val dut = StreamController(1)
        dut.full_bit.simPublic()
        dut.setName("StreamController_1")
        dut
      }

      override def doSim(compiled: SimCompiled[StreamController]): Unit = {
        compiled.doSim("SingleDepth")(dut => {
          dut.clockDomain.forkStimulus(period = 10)
          dut.clockDomain.assertReset()
          if (dut.en.length != 1)
            simFailure(s"Illegal en length: ${dut.en.length}, desired 1")

          def b2i(b: Boolean) = if (b) 1 else 0

          def failIfNot(iready: Boolean, ovalid: Boolean, en0: Boolean, full_bit: Boolean): Unit = {
            if (
              dut.iready.toBoolean != iready ||
              dut.ovalid.toBoolean != ovalid ||
              dut.en(0).toBoolean != en0 ||
              dut.full_bit.toBoolean != full_bit
            )
              simFailure(
                s"Illegal state: " +
                  s"ir,ov,en(0),fb = ${b2i(dut.iready.toBoolean)},${b2i(dut.ovalid.toBoolean)},${b2i(
                      dut.en(0).toBoolean
                    )},${b2i(dut.full_bit.toBoolean)}, " +
                  s"want ${b2i(iready)},${b2i(ovalid)},${b2i(en0)},${b2i(full_bit)}"
              )
          }

          dut.ivalid #= false
          dut.oready #= false
          dut.clockDomain.waitSampling()
          // < - x
          // x - x
          failIfNot(iready = true, ovalid = false, en0 = false, full_bit = false)
          dut.clockDomain.waitActiveEdge()
          // < - x
          // x - x
          failIfNot(iready = true, ovalid = false, en0 = false, full_bit = false)
          // < - x
          // x - x
          dut.ivalid #= true
          dut.oready #= false
          dut.clockDomain.waitSampling()
          // < - x
          // > - x
          failIfNot(iready = true, ovalid = false, en0 = true, full_bit = false)
          dut.clockDomain.waitActiveEdge()
          // x + >
          // > + x
          failIfNot(iready = false, ovalid = true, en0 = false, full_bit = true)
          dut.clockDomain.waitActiveEdge()
          // x + >
          // > + x
          failIfNot(iready = false, ovalid = true, en0 = false, full_bit = true)
          dut.ivalid #= true
          dut.oready #= true
          dut.clockDomain.waitSampling()
          // < + >
          // > + <
          failIfNot(iready = true, ovalid = true, en0 = true, full_bit = true)
          dut.clockDomain.waitActiveEdge()
          // < + >
          // > + <
          failIfNot(iready = true, ovalid = true, en0 = true, full_bit = true)
          dut.clockDomain.waitActiveEdge()
          // < + >
          // > + <
          failIfNot(iready = true, ovalid = true, en0 = true, full_bit = true)
          dut.ivalid #= false
          dut.oready #= true
          dut.clockDomain.waitSampling()
          // < + >
          // x + <
          failIfNot(iready = true, ovalid = true, en0 = false, full_bit = true)
          dut.clockDomain.waitActiveEdge()
          // < - x
          // x - <
          failIfNot(iready = true, ovalid = false, en0 = false, full_bit = false)
          dut.clockDomain.waitActiveEdge()
          // < - x
          // x - <
          failIfNot(iready = true, ovalid = false, en0 = false, full_bit = false)
          dut.ivalid #= true
          dut.oready #= true
          dut.clockDomain.waitSampling()
          // < - x
          // > - <
          failIfNot(iready = true, ovalid = false, en0 = true, full_bit = false)
          dut.clockDomain.waitActiveEdge()
          // < + >
          // > + <
          failIfNot(iready = true, ovalid = true, en0 = true, full_bit = true)
          dut.ivalid #= false
          dut.oready #= false
          dut.clockDomain.waitSampling()
          // x + >
          // x + x
          failIfNot(iready = false, ovalid = true, en0 = false, full_bit = true)
          dut.clockDomain.waitActiveEdge()
          // x + >
          // x + x
          failIfNot(iready = false, ovalid = true, en0 = false, full_bit = true)
          dut.ivalid #= false
          dut.oready #= true
          dut.clockDomain.waitSampling()
          // x + >
          // x + <
          failIfNot(iready = true, ovalid = true, en0 = false, full_bit = true)
          dut.clockDomain.waitActiveEdge()
          // < - x
          // x - x
          failIfNot(iready = true, ovalid = false, en0 = false, full_bit = false)
          simSuccess()
        })
      }
    }
    if (!included) task.excludeFromAll()

    for (depth <- 2 until 33) {
      task = new TestTask[StreamController] {
        private var full_bits = mutable.ListBuffer[Boolean]().appendedAll(Array.fill(depth)(false))
        override def construct(): StreamController = StreamController(depth)
        override def doSim(compiled: SimCompiled[StreamController]): Unit = {
          compiled.doSim("bits")(dut => {
            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.assertReset()
            (0 until 1000).foreach(step => {
              dut.ivalid #= Random.nextBoolean()
              dut.oready #= Random.nextBoolean()
              dut.clockDomain.waitActiveEdge()
              if (dut.ovalid.toBoolean && dut.oready.toBoolean) {
                if (!full_bits.last)
                  simFailure("Output when last stage is empty")
                full_bits = full_bits.init.prepended(false)
              } else {
                for (i <- (1 until full_bits.length).reverse) {
                  if (!full_bits(i) && full_bits(i - 1)) {
                    full_bits(i) = true
                    full_bits(i - 1) = false
                  }
                }
              }
              if (dut.ivalid.toBoolean && dut.iready.toBoolean) {
                if (full_bits.head)
                  simFailure("Input when first stage is full")
                full_bits(0) = true
              }
            })

            simSuccess()
          })

          compiled.doSim("data")(dut => {
            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.assertReset()

            val din = Array.fill(1000)(Random.nextInt())
            var in_pos = 0
            val fifo = Array.fill(depth)(0)
            var out_pos = 0

            while (out_pos != din.length) {
              dut.ivalid #= Random.nextBoolean() && in_pos != din.length
              dut.oready #= Random.nextBoolean()
              dut.clockDomain.waitSampling()
              if (dut.ovalid.toBoolean && dut.oready.toBoolean) {
                if (din(out_pos) != fifo.last) {
                  simFailure(s"Error at data#$out_pos: din=${din(out_pos)}, dout=${fifo.last}")
                }
                out_pos += 1
              }
              for (i <- (1 until depth).reverse) {
                if (dut.en(i).toBoolean)
                  fifo(i) = fifo(i - 1)
              }
              if (dut.en(0).toBoolean) {
                fifo(0) = din(in_pos)
                in_pos += 1
              }
            }
          })
        }
      }
      if (!included) task.excludeFromAll()
    }
  }
}
