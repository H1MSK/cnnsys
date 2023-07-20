package cnnsys.conv_unit

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.{AccessType, AxiLite4BusInterface}

import scala.language.postfixOps

case class ConvAXIController(config: ConvUnitConfig, sizeMapping: SizeMapping) extends Component {
  val ctrl = slave(AxiLite4(config.axiLite4Config))
  val dout = master Stream ConvUnitTask()

  private val busif = AxiLite4BusInterface(ctrl, sizeMapping)

  private val reg_ctrl = busif.newReg("Control register")
  private val field_start = reg_ctrl.field(Bool(), AccessType.RW, 0, "Start trigger")
  private val field_load_param = reg_ctrl.field(Bool(), AccessType.RW, 0, "Param reload trigger")
  reg_ctrl.reserved(6 bits)
  private val field_busy = reg_ctrl.field(Bool(), AccessType.RO, 0, "Busy processing flag")

  private val reg_conf_1 = busif.newReg("Config register 1")
  private val field_width_sel =
    reg_conf_1.field(
      Bits(log2Up(config.supportedInputWidths.length) bits),
      AccessType.RW,
      0,
      "Input width select, from 0 on selecting " + config.supportedInputWidths.mkString(sep = ", ")
    )

  reg_conf_1.reserved(8 - widthOf(field_width_sel) bits)
  private val field_dest = reg_conf_1.field(
    UInt(config.unitOutStreamConfig.destWidth bits),
    AccessType.RW,
    0,
    "Destination ID"
  )
  reg_conf_1.reserved(8 - widthOf(field_dest) bits)
  private val field_do_sigmoid_mul = reg_conf_1.field(
    Bool(),
    AccessType.RW,
    0,
    "Whether to perform x <= x * sigmoid(x) for final output")
  private val field_coop_mode = reg_conf_1.field(
    ConvUnitCoreCooperateMode(),
    AccessType.RW,
    0,
    "Core cooperate mode, from 0 on selecting " + ConvUnitCoreCooperateMode.elements.map(_.toString()).mkString(sep = ", ")
  )

  field_busy := !dout.ready
  dout.payload.load_param := field_load_param
  dout.payload.width_sel := field_width_sel
  dout.payload.dest := field_dest
  dout.payload.do_mul_sigmoid := field_do_sigmoid_mul
  dout.payload.coop_mode := field_coop_mode

  dout.valid.setAsReg() init False
  when(!dout.valid) {
    when(field_start) {
      dout.valid := True
      field_start := False
    }
  }.otherwise {
    when(dout.ready) {
      dout.valid := False
    }
  }
}
