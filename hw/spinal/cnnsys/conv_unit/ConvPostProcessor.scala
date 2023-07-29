package cnnsys.conv_unit

import lib.StreamController.StreamController
import lib.utils.TreeReduce
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ConvPostProcessor(config: ConvUnitConfig) extends Component{
  val coop_mode = in(ConvUnitCoreCooperateMode())
  val do_mul_sigmoid = in Bool()
  val din = slave Stream Vec(SInt(config.coreOutDataBitWidth bits), config.coreOutChannelCount)
  val dout = master Stream new Bundle {
    val data = Bits(config.coreOutDataBitWidth * config.coreOutChannelCount * config.coreCount bits)
    val keep = Bits(config.coreOutDataBitWidth * config.coreOutChannelCount * config.coreCount / 8 bits)
  }

  private val pairController = StreamController(1)
  private val pairAdded = din.payload.indices.filter(_ % 2 == 0).map(i => din.payload(i) +^ din.payload(i + 1))
  private val regPairs = pairAdded.map(RegNextWhen(_, pairController.en(0), init = S(0)))

  private val reduceTree = TreeReduce.withRegs(din.payload.toSeq, 2, (a: SInt, b: SInt) => a + b)

  private val coop_out = Stream(Vec(SInt(config.coreOutDataBitWidth bits), config.coreCount))

  private val requant_out = Stream(Vec(SInt(config.unitOutDataBitWidth bits)))

  pairController.ivalid := False
  pairController.oready := False
  reduceTree.controller.ivalid := False
  reduceTree.controller.oready := False

  when (coop_mode === ConvUnitCoreCooperateMode.independent) {
    coop_out.arbitrationFrom(din)
    coop_out.payload.indices.foreach(i => {
      coop_out.payload(i) := din.payload(i)
    })
    dout.payload.keep := dout.payload.keep.getAllTrue
  }.elsewhen(coop_mode === ConvUnitCoreCooperateMode.pairExtend) {
    coop_out.valid := pairController.ovalid
    pairController.oready := coop_out.ready
    pairController.ivalid := din.valid
    din.ready := pairController.iready

    regPairs.indices.foreach(i => {
      coop_out.payload(i) := regPairs(i)
      coop_out.payload(i + regPairs.length) := coop_out.payload(i).getZero
    })
    dout.payload.keep.setAllTo(False)
    dout.payload.keep(0 until dout.payload.keep.getBitsWidth / 2).setAllTo(True)
  }.elsewhen(coop_mode === ConvUnitCoreCooperateMode.fullExtend) {
    coop_out.valid := reduceTree.controller.ovalid
    reduceTree.controller.oready := coop_out.ready
    reduceTree.controller.ivalid := din.valid
    din.ready := reduceTree.controller.iready

    dout.payload.keep.setAllTo(False)
    dout.payload.keep(0 until dout.payload.keep.getBitsWidth / config.coreCount).setAllTo(True)
  }.otherwise {
    coop_out.valid := False
    dout.payload.data := B(0)
    dout.payload.keep := B(0)
  }
  dout.payload.data := coop_out.payload.asBits
}
