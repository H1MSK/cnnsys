package cnnsys

import cnnsys.conv_unit.ConvUnitConfig
import cnnsys.matmul_unit.MatMulUnitConfig
import spinal.lib.bus.amba4.axilite.AxiLite4Config
import spinal.lib.bus.amba4.axis.Axi4StreamConfig

class UnitConfig {
  val axiLite4Config = AxiLite4Config(
    addressWidth = 10,
    dataWidth = 32
  )
  var addTreeRegisterDistance: Int = 3
  var addTreeExtendBitwidth: Boolean = true
  var addTreeSaturate: Boolean = true
}

object UnitConfig {
  val conv_config = ConvUnitConfig.default
  val matmul_config = MatMulUnitConfig.default
}
