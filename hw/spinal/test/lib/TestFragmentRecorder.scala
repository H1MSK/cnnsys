package test.lib

import _root_.lib.FragmentRecorder
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import test.{TestTask, TestTaskGenerator}

import scala.language.postfixOps
import scala.util.Random

object TestFragmentRecorder extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    new TestTask[BenchFragmentRecorder](included) {
      override def construct(): BenchFragmentRecorder = BenchFragmentRecorder()

      val data: Array[Int] = TestTask.randomUInt(8 bits, 256)
      val lasts: Array[Boolean] = TestTask.randomUInt(1 bit, 255).appended(1).map(_ != 0)

      override def doSim(compiled: SimCompiled[BenchFragmentRecorder]): Unit = {
        compiled.doSim(dut => {
          dut.clockDomain.forkStimulus(10)
          dut.clockDomain.assertReset()

          dut.din_frags.valid #= false
          dut.dout_frags.ready #= false
          dut.clockDomain.waitActiveEdge()
          sleep(1)

          var in_ptr = 0
          var out_ptr = 0

          while (out_ptr != 256) {
            dut.din_frags.valid #= in_ptr != 256
            dut.dout_frags.ready #= true
//            dut.din_frags.valid #= (in_ptr != 256 && Random.nextBoolean())
//            dut.dout_frags.ready #= Random.nextBoolean()
            dut.din_frags.payload.fragment #= (if (in_ptr != 256) data(in_ptr) else 0)
            dut.din_frags.payload.last #= (in_ptr != 256 && lasts(in_ptr))

            dut.clockDomain.waitActiveEdge()
            sleep(1)

            if (dut.dout_frags.ready.toBoolean && dut.dout_frags.valid.toBoolean) {
              val out = dut.dout_frags.payload.fragment
              val last = dut.dout_frags.payload.last
              if (out.toInt != data(out_ptr) || last.toBoolean != lasts(out_ptr)) {
                simFailure(s"Dout doesn't match: want ${data(out_ptr)}, ${lasts(out_ptr)}, but get ${out.toInt}, ${last.toBoolean}\n" +
                  s"in_ptr=$in_ptr, out_ptr=$out_ptr\n" +
                  s"data:  ${data.mkString(", ")}\n" +
                  s"lasts: ${lasts.map(if (_) 1 else 0).mkString(", ")}")
              }
              out_ptr += 1
            }

            if (dut.din_frags.ready.toBoolean && dut.din_frags.valid.toBoolean) in_ptr += 1

          }
        })
      }
    }
  }
}
