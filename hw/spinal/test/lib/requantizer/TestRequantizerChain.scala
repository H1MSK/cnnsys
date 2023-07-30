package test.lib.requantizer

import lib.quantizer.{RequantizerChain, RequantizerConfig, RequantizerParamData}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import test.{TestTask, TestTaskGenerator}

import java.lang.Math.{abs, round}
import scala.language.postfixOps
import scala.util.Random

object TestRequantizerChain extends TestTaskGenerator {
//  override def threadCount: Int = 1

  def requantize(x: Int, param: RequantizerParamData, config: RequantizerConfig): Long = {
      var t = x * (if (config.useScale) param.scale else 1): Long
      if (config.useOffset) {
        t += param.offset
        if (config.useOffsetSaturation && (t >> config.offset_stage_output_bitwidth) != (if (t >= 0) 0 else -1)) {
          val max = 1 << config.offset_stage_output_bitwidth
          t = if (t > 0) (max - 1) else -max
        }
      }
      if (config.useRightShift) t >>= param.shift_count

      val delta = config.shift_stage_output_bitwidth - config.dout_bitwidth
      if (delta <= 0) {
        t
      } else {
        val sign = if (t < 0) -1 else 1
        t = abs(t)
        val answer_bits = t >> delta
        val carry_bit = (t >> (delta - 1)) & 1
        val top_bit = 1L << (config.dout_bitwidth - 1)
        val answer =
          if (answer_bits == (if (sign > 0) top_bit - 1 else top_bit)) answer_bits
          else answer_bits + carry_bit
        answer * sign
      }
  }

  override def prepare(included: Boolean): Unit = {
    for (chain_length <- Array(1, 5)) {
      for (parallel_count <- Array(1, 4)) {
        for (use_feat <- 1 until 16) {
          val use_offset = (use_feat & 1) != 0
          val use_right_shift = (use_feat & 2) != 0
          val use_offset_saturation = (use_feat & 4) != 0
          val use_scale = (use_feat & 8) != 0
          if (use_offset || !use_offset_saturation) {

            val config = RequantizerConfig(
              din_bitwidth = 16,
              dout_bitwidth = 16,
              scale_bitwidth = if (use_scale) 16 else 0,
              useOffset = use_offset,
              useOffsetSaturation = use_offset_saturation,
              useRightShift = use_right_shift
            )
            val params = Array.fill(chain_length)(
              new RequantizerParamData {
                scale = if (use_scale) TestTask.randomSInt(16 bits) else 1
                offset = if (use_offset) TestTask.randomSIntAsLong((if (use_scale) 32 else 16) bits) else 0
                shift_count =
                  if (use_right_shift)
                    Random.nextInt(1 + (if (use_scale) 32 else 16) + (if (use_offset_saturation) 0 else 1))
                  else 0
              }
            )
            //            val dins = Array.fill(256)(Array.fill(chain_length)(TestTask.randomSInt(16 bits, parallel_count)))
            val dins = Array.fill(256)(Array.fill(chain_length)(TestTask.randomSInt(4 bits, parallel_count)))

            val wanted_douts = dins.map(
              _.zip(params)
                .map(t => {
                  val param = t._2
                  t._1.map[Long](x => requantize(x, param, config))
                })
                .toArray
            )

            new TestTask[RequantizerChain](included) {
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
                    if (use_scale && dut.out_param.payload.scale.toLong != p.scale)
                      failed = true
                    if (use_offset && dut.out_param.payload.offset.toLong != p.offset)
                      failed = true
                    if (use_right_shift && dut.out_param.payload.shift_count.toLong != p.shift_count)
                      failed = true

                    if (failed) {
                      def format(p: RequantizerParamData) =
                        s"{ scale=${p.scale}, offset=${p.offset}, shift=${p.shift_count} }"

                      simFailure(
                        s"Out param doesn't match:\n" +
                          "Want " + format(p) + ",\n" +
                          s"but get " + format(new RequantizerParamData {
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
                      for (i <- dins.head.indices) {
                        for (j <- dins.head.head.indices) {
                          if (out(i)(j) != standard(i)(j)) {
                            simFailure(
                              s"Answer doesn't match: out(i)(j)=${out(i)(j)}, standard(i)(j)=${standard(i)(j)}, i=$i, j=$j\n" +
                                s"Config: l: $chain_length, p: $parallel_count, useScale: ${use_scale}, useOffset: ${use_offset}, useOffsetSaturation: ${use_offset_saturation}, useShift: ${use_right_shift}\n" +
                                s"Param(i=$i):\n  scale = ${params(i).scale}, offset = ${params(i).offset}, shift = ${params(i).shift_count}\n" +
                                s"\n  bit width: ${config.bitWidthsToString}\n" +
                                s"In:\n  ${dins(out_batch).map(_.mkString(", ")).mkString("\n  ")}\n" +
                                s"Out:\n  ${out.map(_.mkString(", ")).mkString("\n  ")}\n" +
                                s"Std:\n  ${standard.map(_.mkString(", ")).mkString("\n  ")}\n"
                            )
                          }
                        }
                      }
                      out_batch += 1
                    }
                  }
                })
              }

              private def applyParams(dut: RequantizerChain): Unit = {
                params.foreach(p => {
                  dut.param.valid #= true
                  p.simApplyToBundle(dut.param)
                  dut.clockDomain.waitActiveEdge()
                  sleep(1)
                })
                dut.param.valid #= false
              }
            }
          }
        }
      }
    }
  }
}
