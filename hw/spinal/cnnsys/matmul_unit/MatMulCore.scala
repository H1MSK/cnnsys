package cnnsys.matmul_unit

import lib.{AddTree, ShiftMemory, VectorOperator}
import lib.quantizer.{RequantizerChain, RequantizerParamBundle}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class MatMulCore(config: MatMulUnitConfig) extends Component {
  val din = slave Stream Vec(SInt(config.coreInDataBitWidth bits), config.coreInChannelCount)
  val kernel_data = slave Flow Vec(SInt(config.coreInDataBitWidth bits), config.coreInChannelCount)
  val requantizer_param_in = slave Flow RequantizerParamBundle(config.requantizer_config)
  val bias_data = slave Flow Vec(SInt(config.biasDataBitWidth bits), config.coreOutChannelCount)

  val dout = master Stream Vec(SInt(config.coreOutDataBitWidth bits), config.coreOutChannelCount)

  val kernel = ShiftMemory(
    data_type = Vec(SInt(config.coreInDataBitWidth bits), config.coreInChannelCount),
    size = config.coreOutChannelCount,
    enable_shift_out = false
  )

  val reg_bias: Vec[SInt] = RegNextWhen(bias_data.payload, bias_data.valid, init = bias_data.payload.getZero)

  val multipliers = Array.fill(config.coreOutChannelCount)(
    VectorOperator(
      din_type = SInt(config.coreInDataBitWidth bits),
      kernel_type = SInt(config.coreKernelDataBitWidth bits),
      dout_type = SInt(config.coreMultiplicationResultDataBitWidth bits),
      length = config.coreInChannelCount,
      operation = (a: SInt, b: SInt) => a * b
    )
  )

  val addTrees = Array.fill(config.coreOutChannelCount)(
    AddTree(
      input_bit_width = config.coreAddTreeInDataBitWidth,
      length = config.coreOutChannelCount,
      register_distance = config.addTreeRegisterDistance,
      extend_bitwidth = config.addTreeExtendBitwidth,
      saturate_output = config.addTreeSaturate,
      use_bias = true
    )
  )

  val requantizerChain =
    RequantizerChain(
      chain_length = config.coreOutChannelCount,
      enableChainOut = false,
      parallel_count = 1,
      config = config.requantizer_config
    )

  kernel.din << kernel_data
  requantizerChain.param << requantizer_param_in

  multipliers.indices.foreach(i => {
    val mul = multipliers(i)
    val adder = addTrees(i)
    val rq = requantizerChain
    mul.din.valid := din.valid
    mul.din.payload := din.payload
    mul.static_in := kernel.regs.reverse(i)

    adder.din << mul.dout
    adder.bias := reg_bias(i)

    rq.din.payload(i)(0) := adder.dout.payload
    adder.dout.ready := rq.din.ready
  })
  din.ready := multipliers.head.din.ready
  requantizerChain.din.valid := addTrees.head.dout.valid

  dout.arbitrationFrom(requantizerChain.dout)
  dout.payload := Vec(requantizerChain.dout.payload.map(_(0)))
}
