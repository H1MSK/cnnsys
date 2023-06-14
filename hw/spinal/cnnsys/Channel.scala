package cnnsys

import cnnsys.config.UnitConfig
import spinal.core._
import utils.TreeReduce

import scala.collection.mutable.ListBuffer

case class Channel(config: UnitConfig) extends Component {
  val delay = 1

  val line_width = in UInt(log2Up(config.maxInputWidth) bits)
  val in_data = in UInt(config.inDataBitWidth bits)
  val out_data = out UInt(config.outDataBitWidth * config.coreOutChannelCount bits)
  val en = in Bool()

  val valid = out Bool()

  val in_shift_kernel = in UInt(config.coreOutChannelCount * config.kernelDataBitWidth bits)
  val kernel_shift_en = in Bool()

  val kernel = Seq.fill(config.coreOutChannelCount)(
    KernelMem(config.maxKernelSize * config.maxKernelSize, config.kernelDataBitWidth)
  )
  kernel.indices.foreach(i => {
    kernel(i).shift_in := in_shift_kernel(i * config.kernelDataBitWidth, config.kernelDataBitWidth bits)
    kernel(i).shift_en := kernel_shift_en
  }
  )

  val lineBuffers = Array.fill(config.maxKernelSize - 1)(LineBuffer(config.inDataBitWidth, config.maxInputWidth, config.maxKernelSize))
  val lastLineBuffer = LineBuffer(config.inDataBitWidth, config.maxKernelSize, config.maxKernelSize)
  lineBuffers.foreach(l => {
    l.en := en
    l.line_width := line_width
  })
  lastLineBuffer.en := en

  var last_data = in_data
  if (lineBuffers.nonEmpty) {
    lineBuffers.foreach(p => {
      p.shift_input := last_data
      last_data = p.shift_output
    })
  }
  lastLineBuffer.shift_input := last_data

  var input_nums = ListBuffer[UInt]()
  lineBuffers.foreach(b => {
    (0 until config.maxKernelSize).foreach(j => {
      input_nums += b.outputs(j)
    }
    )
  })
  (0 until config.maxKernelSize).foreach(j => {
    input_nums += lastLineBuffer.outputs(j)
  })

  def calculateChannel(i: Int): UInt = {
    val cur_kernel = kernel(i)
    var out = U(0)
    var t = 0
    TreeReduce(input_nums.indices.map(i => input_nums(i) * cur_kernel.regs(i)), (a: UInt, b: UInt) => a + b)
  }

  (0 until config.coreOutChannelCount).foreach(i => {
    out_data(i * config.outDataBitWidth, config.outDataBitWidth bits) := RegNextWhen(
      calculateChannel(i).resize(4 bits), en
    )
  })
}
