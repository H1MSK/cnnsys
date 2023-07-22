package cnnsys

import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

object ProjectConfig {
  def spinal = SpinalConfig(
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    defaultClockDomainFrequency = FixedFrequency(200 MHz),
//    dumpWave = DumpWaveConfig(depth = 1000),
    onlyStdLogicVectorAtTopLevelIo = true
  )

  def sim = SimConfig
    .withConfig(spinal)
    .withVcdWave
    .workspacePath("sim_out")
    .allOptimisation
}
