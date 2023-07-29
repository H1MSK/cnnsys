package cnnsys.conv_unit

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.Axi4Stream
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

import scala.language.postfixOps

case class ConvCoreParamLoader(config: ConvUnitConfig) extends Component {
  val din = slave(Axi4Stream(config.coreInStreamConfig))

  val kernel_data = master Flow Vec(SInt(config.coreKernelDataBitWidth bits), config.coreInChannelCount)
  val bias_data = master Flow Vec(SInt(config.coreOutDataBitWidth bits), config.coreOutChannelCount)
  val quant_scaler_data = master Flow new Bundle {
    val scaler = SInt(config.requantizerScalerDataBitWidth bits)
    val shifter = SInt(config.requantizerShifterDataBitWidth bits)
  }

  private val input_length = config.coreInStreamConfig.dataWidth * 8
  val reg_store = Reg(Bits(2 * input_length bits)) init 0
  val reg_count = Reg(UInt(log2Up(input_length * 2) bits)) init 0
  val reg_current_out_length = Reg(UInt(log2Up(input_length * 2) bits)) init 0

  val current_dout_valid = Bool()

  din.ready := False
  List(kernel_data, bias_data, quant_scaler_data).foreach(p =>
    p.payload.asBits := reg_store(0 until p.payload.getBitsWidth)
  )

  when(din.valid && din.ready) {
    reg_store := reg_store | (din.payload.data.resize(2 * input_length) << reg_count)
    reg_count := reg_count + input_length
  }.elsewhen(current_dout_valid) {
    reg_store := reg_store >> reg_current_out_length
    reg_count := reg_count - reg_current_out_length
  }

  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val state_load_kernel = new State
    val state_load_bias = new State
    val state_load_quant_data = new State

    val data_loading_states = List(state_load_kernel, state_load_bias, state_load_quant_data)
    val next_state = data_loading_states.zip(data_loading_states.tail ++ List(idle))
    val output_stream = Map(
      state_load_kernel -> kernel_data,
      state_load_bias -> bias_data,
      state_load_quant_data -> quant_scaler_data
    )


    idle.whenIsActive {
      din.ready := True
      when(din.valid) {
        goto(state_load_kernel)
      }
    }

    data_loading_states.foreach(state => {
      val sout = output_stream(state)
      state.onEntry {
        reg_current_out_length := sout.payload.getBitsWidth
      }.whenIsActive {
        current_dout_valid := sout.valid

        when(reg_count <= input_length) {
          din.ready := True
        }.otherwise {
          sout.valid := True
        }
      }
    })
  }
}
