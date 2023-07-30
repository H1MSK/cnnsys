package cnnsys.conv_unit

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

import scala.language.postfixOps

case class ConvUnitOutputTrimmer(config: ConvUnitConfig) extends Component {
  val din = slave Stream Fragment(Vec(SInt(config.unitOutDataBitWidth bits), config.coreOutChannelCount * config.coreCount))
  val dout = master Stream Fragment(Vec(SInt(config.unitOutDataBitWidth bits), config.coreOutChannelCount * config.coreCount))
  val line_width_sel =
    (config.supportedInputWidths.length > 1) generate Bits(log2Up(config.supportedInputWidths.length) bits)

  if (config.supportedInputWidths.length > 1) in(line_width_sel)

  if (config.kernelSize == 1) {
    dout << din
  } else {
    val muxList = config.supportedInputWidths.indices.map(i => i -> U(config.supportedInputWidths(i)))
    val width =
      (if (config.supportedInputWidths.length > 1)
         line_width_sel.muxList(
           if (isPow2(muxList.length)) muxList
           else muxList.appended(default -> U(config.supportedInputWidths.last))
         )
       else
         U(config.supportedInputWidths.last)).setName("width")

    val counter =
      RegInit(U(0, log2Up(config.supportedInputWidths.last * (config.kernelSize - 1) + 1) bits)).setName("counter")
    val counterPlusOne = (counter + U(1)).setName("counterPlusOne")

    val zero = counter.getZero.setName("zero")

    val din_fired = din.fire.setName("din_fired")

    din.ready := False
    dout.valid := False
    dout.payload := din.payload

    val lineInitializing = (counterPlusOne < config.kernelSize).setName("lineInitializing")

    val fsm = new StateMachine {
      val starting = new State with EntryPoint
      val running = new State

      starting
        .onEntry {
          counter := zero
        }
        .whenIsActive {
          din.ready := True
          dout.valid := False
          when(din_fired) {
            when(din.last) {
              counter := zero
            }.elsewhen(counterPlusOne === width * (config.kernelSize - 1)) {
              goto(running)
            }.otherwise {
              counter := counterPlusOne
            }
          }
        }

      running
        .onEntry {
          counter := zero
        }
        .whenIsActive {
          din.ready := lineInitializing || dout.ready
          dout.valid := !lineInitializing && din.valid
          when(din_fired) {
            when(din.last) {
              goto(starting)
            }.elsewhen(counterPlusOne === width) {
              counter := zero
            }.otherwise {
              counter := counterPlusOne
            }
          }
        }
    }.setName("fsm")
  }
}
