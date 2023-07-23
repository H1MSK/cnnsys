package cnnsys.conv_unit

import lib.StreamController.StreamController
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** Single in multi out conv calculator
  *
  * {{{
  * // Waveform: https://wavedrom.com/editor.html
  * // Suppose kernel is 1x3 "5, 7, 11", dout.ready==1
  * {signal: [
  *   {name: 'clk',                  wave: 'p............'},
  *   {name: 'channel.din.d',        wave: 'x.34x5x67x...', data: ['1', '2', '3', '1', '1']},
  *   {name: 'channel.din.v',        wave: '0.11010110...'},
  *   {},
  *   {name: 'lineBuffer.din.d',     wave: 'x.34x5x67x...', data: ['1', '2', '3', '1', '1']},
  *   {name: '=.=.v',                wave: '0.11010110...'},
  *   {name: '=.exports',            wave: '=..34.5.67...', data: ['x,x,x', 'x,x,1', 'x,1,2', '1,2,3', '2,3,1', '3,1,1']},
  *   {},
  *   {name: 'channel.input_nums',   wave: '=..34.5.67...', data: ['x,x,x', 'x,x,1', 'x,1,2', '1,2,3', '2,3,1', '3,1,1']},
  *   {name: 'mulAddTree.din.d',     wave: '=..34.5.67...', data: ['x,x,x', 'x,x,1', 'x,1,2', '1,2,3', '2,3,1', '3,1,1']},
  *   {name: '=.din.v',              wave: '0..11010110..'},
  *   {name: '=.products',           wave: '=..34.5.67...', data: ['x,x,x', 'x,x,5', 'x,7,10', '5,14,33', '...', '15,7,11'], phase: -0.5},
  *   {name: '=.reg_products',       wave: '=...34.5.67..', data: ['x,x,x', 'x,x,5', 'x,7,10', '5,14,33', '...', '15,7,11']},
  *   {name: '=.sum01',              wave: '=...34.5.67..', data: ['x', 'x', 'x', '19', '31', '22'], phase: -0.3},
  *   {name: '=.result',             wave: '=...34.5.67..', data: ['x', 'x', 'x', '52', '42', '33'], phase: -0.6},
  *   {name: '=.final_result',       wave: '=....34.5.67.', data: ['x', 'x', 'x', '52', '42', '33']},
  *   {name: '=.reg_final_result',   wave: '=....34.5.67.', data: ['x', 'x', 'x', '52', '42', '33']},
  *   {name: '=.dout.d',             wave: '=....34.5.67.', data: ['x', 'x', 'x', '52', '42', '33']},
  *   {name: '=.dout.v',             wave: '0....11010110', data: ['x', 'x', 'x', '52', '42', '33']},
  * ]}
  * }}}
  */
case class ConvChannel(config: ConvUnitConfig) extends Component {
  val line_width_sel = in Bits(log2Up(config.supportedInputWidths.length) bits)
  val din = slave Stream SInt(config.unitInDataBitWidth bits)
  val dout = master Stream Vec(Vec(SInt(config.productDataBitWidth bits), config.kernelSize * config.kernelSize), config.coreOutChannelCount)

  val kernel_din = slave Flow SInt(config.unitKernelDataBitWidth bits)

  private val kernel = Seq.fill(config.coreOutChannelCount)(
    ConvKernelMem(config.kernelSize * config.kernelSize, config.unitKernelDataBitWidth)
  )
  private var last_din = kernel_din
  kernel.reverse.foreach(k => {
    last_din >> k.din
    last_din = k.dout
  })

  private val lineBufferStreamController = StreamController(1)
  lineBufferStreamController << din

  private val lineBuffers = Array.fill(config.kernelSize - 1)(
    ConvLineBuffer(config)
  )
  private val lastLineBuffer =
    ConvLineBuffer(config, hasShiftOutput = false)
  lineBuffers.foreach(_.line_width_sel := line_width_sel)

  private var last_data = din.payload
  if (lineBuffers.nonEmpty) {
    lineBuffers.foreach(p => {
      p.din.valid := lineBufferStreamController.en(0)
      p.din.payload := last_data
      last_data = p.dout
    })
  }
  lastLineBuffer.din.payload := last_data
  lastLineBuffer.din.valid := lineBufferStreamController.en(0)

  private val input_nums = lineBuffers.flatMap(_.exports) ++ lastLineBuffer.exports

  private val vec_inputs = Vec(input_nums)

  assert(vec_inputs.length == kernel.head.regs.length)

  private val convCalculator = Array.fill(config.coreOutChannelCount)(ConvCalculator(config))

  lineBufferStreamController.oready := convCalculator(0).din.ready

  (0 until config.coreOutChannelCount).foreach(i => {
    val cc = convCalculator(i)

    cc.kernel_in := kernel(i).regs
    cc.din.payload := vec_inputs
    cc.din.valid := lineBufferStreamController.ovalid
    dout.payload(i) := cc.dout.payload
    cc.dout.ready := dout.ready
  })

  dout.valid := convCalculator.head.dout.valid
}