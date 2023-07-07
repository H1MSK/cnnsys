package cnnsys.conv_core

import spinal.core
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig, Axi4StreamWidthAdapter}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.{AccessType, AxiLite4BusInterface}
import spinal.lib.fsm._

import scala.language.postfixOps

case class Unit(config: ConvUnitConfig, sizeMapping: SizeMapping) extends Component {
  val ctrl = slave(AxiLite4(config.axiLite4Config))
  val din = slave(Axi4Stream(config.unitInStreamConfig))
  val dout = master(Axi4Stream(config.unitOutStreamConfig))
  val intr = out(Reg(Bool())) init False

  private val cores = Array.fill(config.coreCount)(Core(config))

  private val busif = AxiLite4BusInterface(ctrl, sizeMapping)

  private val reg_ctrl = busif.newReg("Control register")
  private val field_start = reg_ctrl.field(Bool(), AccessType.RW, 0, "Start trigger")
  private val field_load_param = reg_ctrl.field(Bool(), AccessType.RW, 0, "Param reload trigger")
  private val field_busy = reg_ctrl.field(Bool(), AccessType.RO, 0, "Busy processing flag")

  private val reg_conf_1 = busif.newReg("Config register 1")
  private val field_width_sel =
    reg_conf_1.field(
      UInt(log2Up(config.supportedInputWidths.length) bits),
      AccessType.RW,
      0,
      "Input width select, from 0 on selects " + config.supportedInputWidths.mkString(sep = ", ")
    )

  reg_conf_1.reserved(16 - log2Up(config.supportedInputWidths.length) bits)
  private val field_dest = reg_conf_1.field(
    UInt(config.unitOutStreamConfig.destWidth bits),
    AccessType.RW,
    0,
    "Destination ID"
  )

  private val din_stream_sel = RegInit(U(0, 1 bit))
  private val din_as_stream_fragment = din.toBitStreamFragment()
  private val din_demux = StreamDemux(din_as_stream_fragment, din_stream_sel, 2)

  private val in_kernel_stream =
    Stream Fragment Bits(config.coreKernelDataBitWidth * config.coreInChannelCount * config.coreCount bits)
  private val in_kernel_adapter = StreamFragmentWidthAdapter(din_demux(0), in_kernel_stream, order = LOWER_FIRST)
  private val core_kernal_data = in_kernel_stream.payload.fragment.subdivideIn(config.coreCount slices)

  private val in_data_stream =
    Stream Fragment Bits(config.coreInDataBitWidth * config.coreInChannelCount * config.coreCount bits)
  private val in_data_adapter = StreamFragmentWidthAdapter(din_demux(1), in_data_stream, order = LOWER_FIRST)
  private val core_in_data = in_data_stream.payload.fragment.subdivideIn(config.coreCount slices)

  private val cur_line_width_sel = RegInit(cores.head.line_width_sel.getZero)
  private val cur_dout_dest = RegInit(dout.dest.getZero)

  cores.indices.foreach(i => {
    val c = cores(i)
    val kernel_din_slice = core_kernal_data(i).asUInt.subdivideIn(config.coreInChannelCount slices)
    val din_slice = core_in_data(i).asUInt.subdivideIn(config.coreInChannelCount slices)
    c.line_width_sel := cur_line_width_sel
    c.kernel_data.payload := kernel_din_slice
    c.din.payload := din_slice
  })

  private val out_stream = Stream(Bits(config.coreOutDataBitWidth * config.coreOutChannelCount * config.coreCount bits))
  private val dout_as_stream = Stream(Bits(dout.config.dataWidth * 8 bits))
  dout_as_stream.ready := dout.ready
  dout.valid := dout_as_stream.valid
  dout.payload.data := dout_as_stream.payload
  private val dout_width_adapter = StreamWidthAdapter(out_stream, dout_as_stream)

  out_stream.payload := Vec(cores.flatMap(_.dout.payload)).asBits
  dout.payload.dest := cur_dout_dest

  assert(config.coreInDataBitWidth == config.coreKernelDataBitWidth)

  // TODO: parallel mode / addup mode switch
  dout.keep.setAllTo(true)

  field_busy := True
  in_kernel_stream.ready := False
  cores.foreach(_.kernel_data.valid := False)
  in_data_stream.ready := False
  cores.foreach(_.din.valid := False)
  cores.foreach(_.dout.ready := False)
  out_stream.valid := False

  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val param_load = new State
    val calculate = new State

    idle.whenIsActive {
      field_busy := False
      when(field_start) {
        when(field_load_param) {
          goto(param_load)
        }.otherwise {
          goto(calculate)
        }
      }
    }
    idle.onExit {
      cur_dout_dest := field_dest
      cur_line_width_sel := field_width_sel
      field_start := False
      field_load_param := False
      intr := False
    }

    param_load
      .onEntry {
        din_stream_sel := U(0)
      }
      .whenIsActive {
        in_kernel_stream.ready := True
        cores.foreach(_.kernel_data.valid := in_kernel_stream.valid)
        when(in_kernel_stream.last) {
          goto(calculate)
        }
      }

    calculate
      .onEntry {
        din_stream_sel := U(1)
      }
      .whenIsActive {
        in_data_stream.ready := Vec(cores.map(_.din.ready)).reduceBalancedTree(_ & _)
        cores.foreach(_.din.valid := in_data_stream.valid)
        cores.foreach(_.dout.ready := out_stream.ready)
        out_stream.valid := cores.head.dout.valid
        when(in_data_stream.last) {
          goto(idle)
        }
      }
      .onExit {
        intr := True
      }
  }
}
