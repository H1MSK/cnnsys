package cnnsys

import cnnsys.conv_core.{ConvUnitConfig, Core}
import spinal.core.Component
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config}
import spinal.lib.slave

case class System () extends Component {
  val config = ConvUnitConfig()

  val io_ctrl = slave(AxiLite4(AxiLite4Config(dataWidth = 32, addressWidth = 16)))

}

object GenVerilog extends App {
  ProjectConfig.spinal.generateVerilog(Core(ConvUnitConfig())).printPruned()
}

object GenVhdl extends App {
  ProjectConfig.spinal.generateVhdl(Core(ConvUnitConfig())).printPruned()
}
