package cnnsys

import spinal.lib.bus.amba4.axilite.AxiLite4Config
import spinal.lib.bus.amba4.axis.Axi4StreamConfig

class UnitConfig {
  val axiLite4Config = AxiLite4Config(
    addressWidth = 10,
    dataWidth = 32
  )
}
