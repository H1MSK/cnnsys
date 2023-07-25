package test.lib.requantizer

import lib.quantizer.{RequantizerChain, RequantizerConfig}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import test.{TestTask, TestTaskGenerator}

import java.lang.Math.{abs, round}
import scala.language.postfixOps

object TestRequantizerChain extends TestTaskGenerator {
  override def prepare(included: Boolean): Unit = {
    for (chain_length <- Array(1, 5)) {
      for (parallel_count <- Array(1, 4)) {
        for (use_feat <- 0 until 16) {
          val use_offset = (use_feat & 1) != 0
          val use_right_shift = (use_feat & 2) != 0
          val use_offset_saturation = (use_feat & 4) != 0
          val use_scale = (use_feat & 8) != 0
          val config = RequantizerConfig(
            din_bitwidth = 16,
            dout_bitwidth = 16,
            scale_bitwidth = if (use_scale) 16 else 0,
            useOffset = use_offset,
            useOffsetSaturation = use_offset_saturation,
            useRightShift = use_right_shift
          )
          class Param {
            var scale: Int = 0
            var offset: Int = 0
            var shift_count: Int = 0
          }
          val params = Array.fill(chain_length)(
            new Param {
              scale = if (use_scale) TestTask.randomSInt(16 bits) else -1
              offset = if (use_offset) TestTask.randomUInt(32 bits) else 0
              shift_count = if (use_right_shift) TestTask.randomUInt((if (use_scale) 5 else 4) bits) else 0
            }
          )
          val dins = Array.fill(16)(Array.fill(chain_length)(TestTask.randomSInt(16 bits, parallel_count)))

          val wanted_douts = dins.map(
            _.zip(params)
              .map(t => {
                val param = t._2
                t._1.map[Long](x => {
                  var t = x * (if (use_scale) param.scale else 1): Long
                  if (use_offset) t += param.offset
                  if (use_offset_saturation && (t >> (if (use_scale) 32 else 16)) != 0) {
                    t = t.toInt
                  }
                  if (use_right_shift) t >>= param.shift_count

                  val sign = if (t < 0) -1 else 1
                  t = abs(t)
                  if (use_scale)
                    t = (t >> 16) + (if (((t: Long) & (1 << 15: Long)) != 0) 1 else 0)
                  t * sign
                })
              })
              .toArray
          )

          val task = new TestTask[RequantizerChain] {
            override def construct(): RequantizerChain = RequantizerChain(
              chain_length = chain_length,
              enableChainOut = true,
              parallel_count = parallel_count,
              config = config
            )

            override def doSim(compiled: SimCompiled[RequantizerChain]): Unit = {
              compiled.doSim("ParamShift")(dut => {
                dut.clockDomain.forkStimulus(10)
                dut.clockDomain.assertReset()

                dut.din.valid #= false
                dut.dout.ready #= false
                dut.param.valid #= false

                applyParams(dut)

                params.foreach(p => {
                  var failed = false
                  dut.param.valid #= true
                  if (use_scale && dut.out_param.payload.scale.toInt != p.scale)
                    failed = true
                  if (use_offset && dut.out_param.payload.offset.toInt != p.offset)
                    failed = true
                  if (use_right_shift && dut.out_param.payload.shift_count.toInt != p.shift_count)
                    failed = true

                  if (failed) {
                    def format(p: Param) = s"{ scale=${p.scale}, offset=${p.offset}, shift=${p.shift_count} }"
                    simFailure(
                      s"Out param doesn't match:\n" +
                        "Want " + format(p) + ",\n" +
                        s"but get " + format(new Param {
                          scale = if (use_scale) dut.out_param.payload.scale.toInt else -1
                          offset = if (use_offset) dut.out_param.payload.offset.toInt else 0
                          shift_count = if (use_right_shift) dut.out_param.payload.shift_count.toInt else 0
                        }) +
                        s"\nConfig: l: $chain_length, p: $parallel_count, useScale: ${use_scale}, useOffset: ${use_offset}, useOffsetSaturation: ${use_offset_saturation}, useShift: ${use_right_shift}\n"
                    )
                  }

                  dut.clockDomain.waitActiveEdge()
                  sleep(1)
                })
              })

              compiled.doSim("DataPath")(dut => {
                dut.clockDomain.forkStimulus(10)
                dut.clockDomain.assertReset()

                dut.din.valid #= false
                dut.dout.ready #= false
                dut.param.valid #= false

                applyParams(dut)

                var in_batch = 0
                var out_batch = 0
                while (out_batch != dins.length) {
                  dut.dout.ready #= true
                  dut.din.valid #= true
                  dut.din.payload.zip(dins(in_batch)).foreach(t => t._1.zip(t._2).foreach(t => t._1 #= t._2))

                  dut.clockDomain.waitActiveEdge()
                  sleep(1)

                  if (dut.din.ready.toBoolean && dut.din.valid.toBoolean && in_batch + 1 < dins.length) in_batch += 1

                  if (dut.dout.valid.toBoolean && dut.dout.ready.toBoolean) {
                    val out = dut.dout.payload.map(_.map(_.toInt))
                    val standard = wanted_douts(out_batch)
                    out_batch += 1
                    for (i <- dins.head.indices) {
                      for (j <- dins.head.head.indices) {
                        if (out(i)(j) != standard(i)(j)) {
                          simFailure(
                            s"Answer doesn't match: out(i)(j)=${out(i)(j)}, standard(i)(j)=${standard(i)(j)}, i=$i, j=$j\n" +
                              s"Config: l: $chain_length, p: $parallel_count, useScale: ${use_scale}, useOffset: ${use_offset}, useOffsetSaturation: ${use_offset_saturation}, useShift: ${use_right_shift}\n" +
                              s"Param(i=$i): scale = ${params(i).scale}, offset = ${params(i).offset}, shift = ${params(i).shift_count}\n" +
                              s"Out:\n  ${out.map(_.mkString(", ")).mkString("\n  ")}\n" +
                              s"Std:\n  ${standard.map(_.mkString(", ")).mkString("\n  ")}\n"
                          )
                        }
                      }
                    }
                  }
                }
              })
            }

            private def applyParams(dut: RequantizerChain): Unit = {
              params.foreach(p => {
                dut.param.valid #= true
                if (use_scale)
                  dut.param.payload.scale #= p.scale
                if (use_offset)
                  dut.param.payload.offset #= p.offset
                if (use_right_shift)
                  dut.param.payload.shift_count #= p.shift_count
                dut.clockDomain.waitActiveEdge()
                sleep(1)
              })
              dut.param.valid #= false
            }
          }

          if (!included) task.excludeFromAll()
        }
      }
    }
  }
}
