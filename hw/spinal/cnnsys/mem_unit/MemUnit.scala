package cnnsys.mem_unit

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axis.Axi4Stream
import spinal.lib.bus.regif.{AccessType, AxiLite4BusInterface}

case class MemUnit(config: MemUnitConfig, sizeMapping: SizeMapping) extends Component {
  val ctrl = slave(AxiLite4(config.axiLite4Config))
//  val din = slave(Axi4Stream(config.unitInStreamConfig))
//  val dout = master(Axi4Stream(config.unitOutStreamConfig))
  val intr = out(Reg(Bool())) init False

  private val busif = AxiLite4BusInterface(ctrl, sizeMapping)

  private val reg_ctrl = busif.newReg("Control register")
  private val field_start = reg_ctrl.field(Bool(), AccessType.RW, 0, "Start trigger")
  reg_ctrl.reserved(7 bits)
  private val field_busy = reg_ctrl.field(Bool(), AccessType.RO, 0, "Busy processing flag")

  private val reg_conf_1 = busif.newReg("Config register 1")
//  private val field_start_pos = reg_conf_1.field(UInt(20 bits), AccessType.RW, )
}
