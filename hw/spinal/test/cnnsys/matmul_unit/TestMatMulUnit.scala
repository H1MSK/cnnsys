package test.cnnsys.matmul_unit

import cnnsys.matmul_unit.{MatMulUnit, MatMulUnitConfig}
import lib.quantizer.RequantizerParamData
import spinal.core.IntToBuilder
import spinal.core.sim._
import test.lib.TestAddTree
import test.lib.requantizer.TestRequantizerChain
import test.{TestTask, TestTaskGenerator, TestTrait}

import scala.language.postfixOps
import scala.util.Random

object TestMatMulUnit extends TestTaskGenerator with TestTrait {
  def loadKernelDataAndCheck(dut: MatMulUnit, kernelData: Array[Array[Int]]): Unit = {
    val config = dut.config
    assert(kernelData.length == config.outputWidth)
    assert(kernelData.head.length == config.inputWidth)
    assert(
      kernelData
        .map(
          _.map(x => TestTask.inRangeOfSInt(config.coreKernelDataBitWidth bits, x))
            .reduce(_ && _)
        )
        .reduce(_ && _)
    )

    kernelData.foreach(data => {
      sleep(1)
      dut.kernel_data_stream.payload.data #= toBigInt(config.unitKernelDataBitWidth, data)
      dut.kernel_data_stream.valid #= true
      do {
        dut.clockDomain.waitActiveEdge()
      } while (!(dut.kernel_data_stream.valid.toBoolean && dut.kernel_data_stream.ready.toBoolean))
    })
    sleep(1)
    dut.kernel_data_stream.valid #= false

    checkLoadedKernelData(dut, kernelData)
  }

  def checkLoadedKernelData(dut: MatMulUnit, kernelData: Array[Array[Int]]): Unit = {
    val config = dut.config
    val stored = dut.core.kernel.regs.reverse.map(_.map(_.toInt))

    (0 until config.outputWidth).foreach(oc => {
      (0 until config.inputWidth).foreach(ic => {
        if (kernelData(oc)(ic) != stored(oc)(ic))
          simFailure(
            "Kernel data load failed\n" +
              s"Want:\n${kernelData.map(_.mkString("(", ", ", ")")).mkString("[", ",\n", "]")}\n" +
              s"Get:\n${stored.map(_.mkString("(", ", ", ")")).mkString("[", ",\n", "]")}"
          )
      })
    })
  }

  def loadRequantizerDataAndCheck(dut: MatMulUnit, requantizerData: Array[RequantizerParamData]): Unit = {
    val config = dut.config
    val rc = config.requantizer_config
    assert(requantizerData.length == config.outputWidth)
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

  private def checkLoadedRequantizerParamData(dut: MatMulUnit, requantizerData: Array[RequantizerParamData]): Unit = {
    val config = dut.config
    val rc = config.requantizer_config
    val stored_data = dut.core.requantizerChain.chain.map(b =>
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
        val rc = dut.core.requantizerChain.config

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

  def loadBiasDataAndCheck(dut: MatMulUnit, biasData: Array[Int]): Unit = {
    val config = dut.config
    assert(biasData.length == config.outputWidth)
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

  private def checkLoadedBiasData(dut: MatMulUnit, biasData: Array[Int]): Unit = {
    val stored = dut.core.reg_bias.map(_.toInt).toArray

    if (!(biasData sameElements stored)) {
      simFailure(
        s"Bias data load failed.\nWant:${biasData.mkString("(", ", ", s")\nGet: ${stored.mkString("(", ", ", ")")}")}"
      )
    }
  }

  case class Parameters(config: MatMulUnitConfig) {
    var kernel = Array.fill(config.outputWidth)(
      TestTask.randomSInt(config.unitKernelDataBitWidth bits, config.inputWidth)
    )

    var requants = Array.fill(config.outputWidth)(
      new RequantizerParamData {
        scale = TestTask.randomSIntAsLong(config.requantizer_config.scale_bitwidth bits)
        offset = TestTask.randomSIntAsLong(config.requantizer_config.offset_bitwidth bits)
        // To make shift_count smaller, upper bound is set to bitwidth rather than (1 << bitwidth)
        shift_count = Random.nextInt(config.requantizer_config.shift_bitwidth)
      }
    )

    var biases = TestTask.randomSInt(config.biasDataBitWidth bits, config.outputWidth)

    def ApplyToDut(dut: MatMulUnit): Unit = {
      loadKernelDataAndCheck(dut, kernel)
      loadRequantizerDataAndCheck(dut, requants)
      loadBiasDataAndCheck(dut, biases)
    }

  }

  def matmul(inputs: Array[Array[Int]], param: Parameters, print: Boolean = false): Array[Array[Int]] = {
    var print_fun = (x: String) => {}
    if (print) print_fun = (x: String) => println(x)
    val config = param.config
    inputs.map(x => {
      param.kernel.zipWithIndex
        .map(t => {
          val k = t._1
          val i = t._2
          val added = TestAddTree.addUp(
            k.zip(x)
              .map(t => t._1 * t._2)
              .appended(param.biases(i))
              .map(x => {
                print_fun(s"AddIn: ${x}")
                x
              }),
            config.addTreeExtendBitwidth,
            config.addTreeSaturate,
            config.coreAddTreeInDataBitWidth
          )
          print_fun(s"Added: $added")
          added
        })
        .zipWithIndex
        .map(t => {
          val requanted = TestRequantizerChain
            .requantize(t._1, param.requants(t._2), param.config.requantizer_config, print = print)
            .toInt
          print_fun(s"Requanted: $requanted")
          requanted
        })
    })
  }

  override def prepare(included: Boolean): Unit = {
    val config = new MatMulUnitConfig() {
      inputWidth = 2
      outputWidth = 2
    }
    new TestTask[MatMulUnit](included) {
      override def construct(): MatMulUnit = {
        val dut = MatMulUnit(config)
        dut.core.kernel.regs.simPublic()
        dut.core.requantizerChain.chain.foreach(_.param.simPublic())
        dut.core.reg_bias.simPublic()
        dut
      }

      override def doSim(compiled: SimCompiled[MatMulUnit]): Unit = {
        compiled.doSim(dut => {
          dut.clockDomain.forkStimulus(10)
          dut.clockDomain.assertReset()

          dut.din_stream.valid #= false
          dut.dout.ready #= false
          dut.kernel_data_stream.valid #= false
          dut.requantizer_param_stream.valid #= false
          dut.bias_data.valid #= false

          (0 until 16).foreach(iter_param => {
            val params = Parameters(config)

            params.ApplyToDut(dut)

            val inputs = Array.fill(1024)(
              TestTask.randomSInt(config.coreInDataBitWidth bits, config.inputWidth)
            )

            val standards = matmul(inputs, params)

            var in_ptr = 0
            var out_ptr = 0
            var next_input = true
            while (out_ptr != standards.length) {
              sleep(1)
              dut.dout.ready #= Random.nextBoolean()
              dut.din_stream.valid #= (in_ptr < inputs.length || !next_input) && Random.nextBoolean()

              if (next_input) {
                if (in_ptr < inputs.length) {
                  next_input = false
                  dut.din_stream.data #= toBigInt(config.coreInDataBitWidth, inputs(in_ptr))
                  in_ptr += 1
                  dut.din_stream.last #= (in_ptr == inputs.length)
                } else {
                  dut.din_stream.data #= 0
                  dut.din_stream.last #= false
                }
              }

              dut.clockDomain.waitActiveEdge()
              if (dut.din_stream.ready.toBoolean && dut.din_stream.valid.toBoolean) {
                next_input = true
              }
              if (dut.dout.ready.toBoolean && dut.dout.valid.toBoolean) {
                val out = dut.dout.data.toBigInt
                val std = toBigInt(config.unitOutDataBitWidth, standards(out_ptr))

                if (out != std) {
                  sleep(1)
                  println(s"Reproduce: ")
                  matmul(Array(inputs(out_ptr)), params, print = true)
                  simFailure(
                    "Unexpected dout payload: " +
                      s"Want ${standards(out_ptr).mkString("(", ", ", ")")}, " +
                      s"but get ${fromBigInt(config.unitOutDataBitWidth, out).mkString("(", ", ", ")")}\n" +
                      s"Params:\nout_ptr = $out_ptr, in_ptr = $in_ptr\n" +
                      s"input:\n${inputs(out_ptr).mkString("(", ", ", ")")}\n" +
                      s"kernels:\n${params.kernel.map(_.mkString("(", ", ", ")")).mkString("\n")}\n" +
                      s"biases:\n${params.biases.mkString("(", ", ", ")")}\n" +
                      s"requantizers:\n${params.requants.mkString("\n")}"
                  )
                }

                out_ptr += 1
              }
            }
          })
        })
      }
    }
  }
}
