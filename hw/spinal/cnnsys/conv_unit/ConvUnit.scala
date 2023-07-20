package cnnsys.conv_unit

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamSimpleWidthAdapter}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.fsm._

import scala.language.postfixOps

case class ConvUnit(config: ConvUnitConfig, sizeMapping: SizeMapping) extends Component {
  val ctrl = slave Stream ConvUnitTask()
  val din = slave(Axi4Stream(config.unitInStreamConfig))
  val conf_in = slave(Axi4Stream(config.unitInStreamConfig))
  val dout = master(Axi4Stream(config.unitOutStreamConfig))
  val intr = out(Reg(Bool())) init False

  private val conf_in_resized =
    Axi4Stream(
      conf_in.config.copy(dataWidth = config.unitKernelDataBitWidth * config.coreInChannelCount * config.coreCount / 8)
    )

  private val core_kernal_data = conf_in_resized.payload.data.subdivideIn(config.coreCount slices)

  private val din_resized =
    Axi4Stream(
      din.config.copy(dataWidth = config.unitInDataBitWidth * config.coreInChannelCount * config.coreCount / 8)
    )
  private val core_in_data = din_resized.payload.data.subdivideIn(config.coreCount slices)

  private val cur_width_sel = RegInit(ctrl.payload.width_sel.getZero)
  private val cur_dout_dest = RegInit(ctrl.payload.dest.getZero)
  private val cur_coop_mode = RegInit(ctrl.payload.coop_mode.getZero)
  private val cur_do_mul_sigmoid = RegInit(ctrl.payload.do_mul_sigmoid.getZero)

  private val dout_narrow = Axi4Stream(dout.config.copy(
    dataWidth = config.unitOutDataBitWidth * config.coreOutChannelCount * config.coreCount / 8
  ))

  private val in_kernel_adapter = Axi4StreamSimpleWidthAdapter(conf_in, conf_in_resized)

  private val in_data_adapter = Axi4StreamSimpleWidthAdapter(din, din_resized)

  private val cores = Array.fill(config.coreCount)(ConvCore(config))

  private val post_processor = ConvPostProcessor(config)

  val doutWidthAdapter = Axi4StreamSimpleWidthAdapter(dout_narrow, dout)

  cores.indices.foreach(i => {
    val c = cores(i)
    val kernel_din_slice = core_kernal_data(i).asSInt.subdivideIn(config.coreInChannelCount slices)
    val din_slice = core_in_data(i).asSInt.subdivideIn(config.coreInChannelCount slices)
    c.line_width_sel := cur_width_sel
    c.kernel_data.payload := kernel_din_slice
    c.din.payload := din_slice
  })

  ctrl.ready := False
  conf_in_resized.ready := False
  cores.foreach(_.kernel_data.valid := False)
  din_resized.ready := False
  cores.foreach(_.din.valid := False)
  cores.foreach(_.dout.ready := False)

  post_processor.dout.ready := dout_narrow.ready
  dout_narrow.valid := post_processor.dout.valid
  dout_narrow.payload.data := post_processor.dout.data
  dout_narrow.payload.dest := cur_dout_dest
  dout_narrow.payload.keep := post_processor.dout.keep

  val fsm: StateMachine = new StateMachine {
    val idle = new State with EntryPoint
    val param_load = new State
    val calculate = new State

    idle.whenIsActive {
      ctrl.ready := True
      when(ctrl.valid) {
        when(ctrl.load_param) {
          goto(param_load)
        }.otherwise {
          goto(calculate)
        }
      }
    }
    idle.onExit {
      cur_dout_dest := ctrl.payload.dest
      cur_width_sel := ctrl.payload.width_sel
      cur_coop_mode := ctrl.payload.coop_mode
      cur_do_mul_sigmoid := ctrl.payload.do_mul_sigmoid
      ctrl.ready := False
      intr := False
    }

    param_load
      .whenIsActive {
        conf_in_resized.ready := True
        cores.foreach(_.kernel_data.valid := conf_in_resized.valid)
        when(conf_in_resized.last) {
          goto(calculate)
        }
      }

    calculate
      .whenIsActive {
        din_resized.ready := cores.head.din.ready
        cores.foreach(_.din.valid := din_resized.valid)
        cores.foreach(_.dout.ready := post_processor.din.ready)
        post_processor.din.valid := cores.head.dout.valid
        when(din_resized.last) {
          goto(idle)
        }
      }
      .onExit {
        intr := True
      }
  }
}
