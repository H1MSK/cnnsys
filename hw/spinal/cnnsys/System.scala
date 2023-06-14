package cnnsys

import cnnsys.config.UnitConfig
import spinal.core.Component
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config}

case class System () extends Component {
  val config = UnitConfig()

  val io_ctrl = AxiLite4(AxiLite4Config(dataWidth = 32, addressWidth = 16))
//  val io_data = Axi4(Axi4Config(
//    dataWidth = config.globalDataBitWidth,
//    addressWidth = config.globalDataAddressWidth,
//    idWidth = 0))
  io_ctrl.setAsSlave()
//  io_data.setAsMaster()
  val unit = Unit(UnitConfig(

  ))

  io_ctrl >> unit.io_ctrl
//  io_data << unit.io_data
}

object GenVerilog extends App {
  ProjectConfig.spinal.generateVerilog(System()).printPruned()
}

object GenVhdl extends App {
  ProjectConfig.spinal.generateVhdl(System()).printPruned()
}
