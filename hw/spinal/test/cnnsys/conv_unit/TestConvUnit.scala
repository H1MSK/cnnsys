package test.cnnsys.conv_unit

import cnnsys.conv_unit.{ConvUnit, ConvUnitConfig}
import lib.quantizer.RequantizerParamData
import spinal.core._
import spinal.core.sim._
import test.lib.TestInput2DPadder
import test.lib.requantizer.TestRequantizerChain
import test.{TestTask, TestTaskGenerator}

import scala.language.postfixOps
import scala.util.Random

object TestConvUnit extends TestTaskGenerator {
  def toBigInt(bitWidth: Int, dat: Array[Int]): BigInt =
    dat.foldRight(BigInt(0))((x, ret) => (ret << bitWidth) | (x & ((1 << bitWidth) - 1)))

  def fromBigInt(bitWidth: Int, dat: BigInt): Array[Int] = {
    assert(bitWidth <= 32)
    (0 until ((dat.bitLength + bitWidth - 1) / bitWidth))
      .map(i => ((dat >> (i * bitWidth)) & ((1 << bitWidth) - 1)).toInt)
      .toArray
  }

  def loadKernelDataAndCheck(dut: ConvUnit, kernelData: Array[Array[Array[Int]]]): Unit = {
    val config = dut.config
    assert(kernelData.length == config.unitOutChannelCount)
    assert(kernelData.head.length == config.unitInChannelCount)
    assert(kernelData.head.head.length == config.kernelSize * config.kernelSize)
    assert(
      kernelData
        .map(
          _.map(
            _.map(x => x >= -(1 << config.unitKernelDataBitWidth) && x < (1 << config.unitKernelDataBitWidth))
              .reduce(_ && _)
          ).reduce(_ && _)
        )
        .reduce(_ && _)
    )

    kernelData.foreach(data => {
      (0 until config.kernelSize * config.kernelSize).foreach(k => {
        sleep(1)
        dut.kernel_data_stream.payload.data #= toBigInt(config.unitKernelDataBitWidth, data.map(_(k)))
        dut.kernel_data_stream.valid #= true
        do {
          dut.clockDomain.waitActiveEdge()
        } while (!(dut.kernel_data_stream.valid.toBoolean && dut.kernel_data_stream.ready.toBoolean))
      })
    })
    sleep(1)
    dut.kernel_data_stream.valid #= false

    checkLoadedKernelData(dut, kernelData)
  }

  def checkLoadedKernelData(dut: ConvUnit, kernelData: Array[Array[Array[Int]]]): Unit = {
    val config = dut.config
    val stored = dut.core.channels.map(_.kernel.map(_.regs.map(_.toInt).reverse.toArray).toArray)

    (0 until config.unitOutChannelCount).foreach(oc => {
      (0 until config.unitInChannelCount).foreach(ic => {
        (0 until config.kernelSize * config.kernelSize).foreach(k => {
          if (kernelData(oc)(ic)(k) != stored(ic)(oc)(k))
            simFailure(
              "Kernel data load failed\n" +
                s"Want:\n${kernelData.map(_.map(_.mkString("(", ", ", ")")).mkString("[", ", ", "]")).mkString("\n")}\n" +
                s"Get:\n${stored.map(_.map(_.mkString("(", ", ", ")")).mkString("[", ", ", "]")).mkString("\n")}"
            )
        })
      })
    })
  }

  def loadRequantizerDataAndCheck(dut: ConvUnit, requantizerData: Array[RequantizerParamData]): Unit = {
    val config = dut.config
    val rc = config.requantizer_config
    assert(requantizerData.length == config.unitOutChannelCount)
    requantizerData.foreach(d => {
      assert(
        rc.scale_bitwidth <= 0 || TestTask.inRangeOfSInt(
          rc.scale_bitwidth bits,
          d.scale
        )
      )
      assert(
        !rc.useOffset || TestTask.inRangeOfSInt(rc.offset_bitwidth bits, d.offset)
      )

      assert(rc.scale_bitwidth + rc.offset_bitwidth + rc.shift_bitwidth <= 64)
      dut.requantizer_param_stream.payload.data #=
        ((d.scale & ((1L << rc.scale_bitwidth) - 1)) |
          ((d.offset & ((1L << rc.offset_bitwidth) - 1)) << rc.scale_bitwidth) |
          ((d.shift_count & ((1L << rc.shift_bitwidth) - 1)) << (rc.scale_bitwidth + rc.offset_bitwidth)))
      dut.requantizer_param_stream.valid #= true
      do {
        dut.clockDomain.waitActiveEdge()
      } while (!(dut.requantizer_param_stream.valid.toBoolean && dut.requantizer_param_stream.ready.toBoolean))
    })

    sleep(1)
    dut.requantizer_param_stream.valid #= false

    checkLoadedRequantizerParamData(dut, requantizerData)
  }

  private def checkLoadedRequantizerParamData(dut: ConvUnit, requantizerData: Array[RequantizerParamData]): Unit = {
    val config = dut.config
    val rc = config.requantizer_config
    val stored_data = dut.core.finalRequantizerChain.chain.map(b =>
      new RequantizerParamData {
        scale = if (rc.useScale) b.param.scale.toLong else 0
        offset = if (rc.useOffset) b.param.offset.toLong else 0
        shift_count = if (rc.useRightShift) b.param.shift_count.toLong else 0
      }
    )

    requantizerData
      .zip(stored_data)
      .foreach(t => {
        val in_data = t._1
        val store_data = t._2
        val rc = dut.core.finalRequantizerChain.config

        if (
          (rc.useScale && (in_data.scale != store_data.scale)) ||
          (rc.useOffset && (in_data.offset != store_data.offset)) ||
          (rc.useRightShift && (in_data.shift_count != store_data.shift_count))
        ) {
          simFailure(
            "Requantizer data load failed.\n" +
              s"Want:\n  ${requantizerData.mkString("\n  ")}\n" +
              s"Get:\n  ${stored_data.mkString("\n  ")}"
          )
        }
      })
  }

  def loadBiasDataAndCheck(dut: ConvUnit, biasData: Array[Int]): Unit = {
    val config = dut.config
    assert(biasData.length == config.unitOutChannelCount)
    biasData.foreach(x => {
      assert(TestTask.inRangeOfSInt(config.biasDataBitWidth bits, x))
    })

    dut.bias_data.payload.data #= toBigInt(config.biasDataBitWidth, biasData)
    dut.bias_data.valid #= true

    do {
      dut.clockDomain.waitActiveEdge()
    } while (!(dut.bias_data.valid.toBoolean && dut.bias_data.ready.toBoolean))

    sleep(1)
    dut.bias_data.valid #= false

    checkLoadedBiasData(dut, biasData)
  }

  private def checkLoadedBiasData(dut: ConvUnit, biasData: Array[Int]): Unit = {
    val stored = dut.core.reg_bias.map(_.toInt).toArray

    if (!(biasData sameElements stored)) {
      simFailure(
        s"Bias data load failed.\nWant:${biasData.mkString("(", ", ", s")\nGet: ${stored.mkString("(", ", ", ")")}")}"
      )
    }
  }

  case class Parameters(config: ConvUnitConfig) {
    // kernels(oc)(ic)(0~kernelSize^2-1)
    var kernels = Array.fill(config.unitOutChannelCount)(
      Array.fill(config.unitInChannelCount)(
        TestTask.randomSInt(config.unitKernelDataBitWidth bits, config.kernelSize * config.kernelSize)
      )
    )

    var requants = Array.fill(config.unitOutChannelCount)(
      new RequantizerParamData {
        scale = TestTask.randomSIntAsLong(config.requantizerScalerDataBitWidth bits)
        offset = 0
        shift_count = Random.nextInt(config.requantizer_config.shift_bitwidth)
      }
    )

    var biasData = TestTask.randomSInt(config.biasDataBitWidth bits, config.unitOutChannelCount)

    var padding_size = Random.nextInt(config.maxPaddingSize)
    var padding_data = TestTask.randomSInt(config.unitInDataBitWidth bits)

    var line_width_sel =
      if (config.supportedInputWidths.length > 0) Random.nextInt(config.supportedInputWidths.length) else 0

    def ApplyToDut(dut: ConvUnit): Unit = {
      applyScalarPort(dut)
      loadKernelDataAndCheck(dut, kernels)
      loadRequantizerDataAndCheck(dut, requants)
      loadBiasDataAndCheck(dut, biasData)
    }

    def applyScalarPort(dut: ConvUnit): Unit = {
      if (config.supportedInputWidths.length > 0)
        dut.line_width_sel #= line_width_sel

      if (config.maxPaddingSize > 0) {
        dut.padding_size #= padding_size
        dut.padding_data #= padding_data
      }
    }
  }

  def conv(inputs: Array[Array[Array[Int]]], param: Parameters): Array[Array[Array[Int]]] = {
    val config = param.config

    val input_width = inputs.head.length
    inputs.slice(0, inputs.length - 1).foreach(line => assert(line.length == input_width))
    assert(inputs.last.length <= input_width && inputs.last.length >= config.kernelSize)
    val sel = config.supportedInputWidths.indexOf(input_width)
    assert(sel == param.line_width_sel)

    assert(inputs.length >= config.kernelSize)

    val padded_inputs = TestInput2DPadder.padding[Array[Int]](
      inputs,
      Array.fill(config.unitInChannelCount)(param.padding_data),
      param.padding_size
    )

    (0 to padded_inputs.length - config.kernelSize)
      .map(row => {
        (0 to padded_inputs(row + config.kernelSize - 1).length - config.kernelSize)
          .map(col => {
            (0 until config.unitOutChannelCount)
              .map(oc => {
                TestConvAddTree.addUp(
                  (0 until config.unitInChannelCount)
                    .flatMap(ic => {
                      (0 until config.kernelSize)
                        .flatMap(dr => {
                          (0 until config.kernelSize).map(dc => {
                            padded_inputs(row + dr)(col + dc)(ic)
                          })
                        })
                        .zip(if (config.convFlipKernel) param.kernels(oc)(ic).reverse else param.kernels(oc)(ic))
                        .map(t => t._1 * t._2)
                    })
                    .appended(param.biasData(oc))
                    .toArray,
                  config.convAddTreeExtendBitwidth,
                  config.convAddTreeSaturate,
                  config.convAddTreeInputDataBitWidth
                )
              })
              .toArray
          })
          .toArray
      })
      .toArray
  }

  override def prepare(included: Boolean): Unit = {
    val config = new ConvUnitConfig() {
    }
    new TestTask[ConvUnit](included) {
      override def construct(): ConvUnit = {
        val dut = ConvUnit(config)
        dut.core.channels.foreach(_.kernel.foreach(_.regs.simPublic()))
        dut.core.finalRequantizerChain.chain.foreach(_.param.simPublic())
        dut.core.reg_bias.simPublic()
        dut
      }

      def calculateSim(dut: ConvUnit): Unit = {
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.assertReset()
        dut.line_width_sel #= 0
        dut.din_stream.valid #= false
        dut.kernel_data_stream.valid #= false
        dut.requantizer_param_stream.valid #= false
        dut.bias_data.valid #= false
        dut.dout.ready #= false

        val parameters = Parameters(config)

        parameters.ApplyToDut(dut)

        config.supportedInputWidths.indices.foreach(line_width_sel => {
          (0 until config.maxPaddingSize).foreach(padding_size => {

            parameters.line_width_sel = line_width_sel
            parameters.padding_size = padding_size
            parameters.padding_data = TestTask.randomSInt(config.unitInDataBitWidth bits)
            parameters.applyScalarPort(dut)

            val width = config.supportedInputWidths(line_width_sel)
            val inputs = Array.fill(width + 2)(
              Array.fill(width)(
                TestTask.randomSInt(config.unitInDataBitWidth bits, config.unitInChannelCount)
              )
            )
            val conv_outputs = conv(inputs, parameters)
            val outputs = conv_outputs.map(
              _.map(p =>
                p.indices
                  .map(oc =>
                    TestRequantizerChain
                      .requantize(p(oc), parameters.requants(oc), parameters.config.requantizer_config)
                      .toInt
                  )
                  .toArray
              )
            )

            val flatted_inputs = inputs.flatten
            val flatted_outputs = outputs.flatten

            var in_ptr = 0
            var out_ptr = 0
            var next_input = true

            checkLoadedKernelData(dut, parameters.kernels)
            checkLoadedRequantizerParamData(dut, parameters.requants)
            checkLoadedBiasData(dut, parameters.biasData)

            while (out_ptr != flatted_outputs.length) {
              sleep(1)
              dut.din_stream.valid #= (in_ptr < flatted_inputs.length || !next_input) && Random.nextBoolean()
              dut.dout.ready #= Random.nextBoolean()
              if (next_input) {
                if (in_ptr < flatted_inputs.length) {
                  next_input = false
                  dut.din_stream.data #= toBigInt(config.unitInDataBitWidth, flatted_inputs(in_ptr))
                  in_ptr += 1
                  dut.din_stream.last #= (in_ptr == flatted_inputs.length)
                } else {
                  dut.din_stream.data #= 0
                  dut.din_stream.last #= false
                }
              }

              dut.clockDomain.waitActiveEdge()
              if (dut.din_stream.ready.toBoolean && dut.din_stream.valid.toBoolean) next_input = true
              if (dut.dout.ready.toBoolean && dut.dout.valid.toBoolean) {
                val out = dut.dout.data.toBigInt
                val std = toBigInt(config.unitOutDataBitWidth, flatted_outputs(out_ptr))
                if (out != std)
                  simFailure(
                    "Unexpected dout payload: " +
                      s"Want ${flatted_outputs(out_ptr).mkString("(", ", ", ")")}, " +
                      s"but get ${fromBigInt(config.unitOutDataBitWidth, out).mkString("(", ", ", ")")}\n" +
                      s"Params:\nout_ptr = $out_ptr, in_ptr = $in_ptr, width = $width, padsize = $padding_size, paddata = ${parameters.padding_data}\n" +
                      s"kernels:\n${parameters.kernels.indices
                          .map(oc => s"#(oc=$oc): ${parameters.kernels(oc).map(_.mkString("(", ", ", ")")).mkString("{", ", ", "}")}")
                          .mkString("\n")}\n" +
                      s"inputs:\n${inputs.map(_.map(_.mkString("(", ", ", ")")).mkString("{", "|", "}")).mkString("\n")}\n" +
                      s"outputs:\n${outputs.map(_.map(_.mkString("(", ", ", ")")).mkString("{", "|", "}")).mkString("\n")}\n"
                  )

                out_ptr += 1
                if (dut.dout.last.toBoolean != (out_ptr == flatted_outputs.length)) {
                  simFailure(
                    "Unexpected dout last flag: " +
                      s"want ${out_ptr == flatted_outputs.length}, but get ${dut.dout.last.toBoolean}\n" +
                      s"Params:\nout_ptr = $out_ptr, in_ptr = $in_ptr\n" +
                      s"kernels:\n${parameters.kernels.indices
                          .map(oc => s"#(oc=$oc): ${parameters.kernels(oc).map(_.mkString("(", ", ", ")")).mkString("{", ", ", "}\n")}")}\n" +
                      s"inputs:\n${inputs.map(_.map(_.mkString("(", ", ", ")")).mkString("{", "|", "}")).mkString("\n")}\n" +
                      s"outputs:\n${outputs.map(_.map(_.mkString("(", ", ", ")")).mkString("{", "|", "}")).mkString("\n")}\n"
                  )
                }
              }
            }

            dut.din_stream.valid #= false
            dut.dout.ready #= false
          })
        })
      }

      override def doSim(compiled: SimCompiled[ConvUnit]): Unit = {
        compiled.doSim("CalculateSim")(calculateSim)
      }
    }
  }
}
