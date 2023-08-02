package cnnsys

import cnnsys.conv_unit.{ConvUnit, ConvUnitConfig}
import cnnsys.matmul_unit.{MatMulUnit, MatMulUnitConfig}
import lib.utils.XilinxBusTagger
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class System() extends Component with UnitTrait {
  val conv_config = ConvUnitConfig.default
  val matmul_config = MatMulUnitConfig.default

  def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)
  def assertMinEqualsGcd(x: Int, y: Int): Unit = {
    assert(Math.min(x, y) == gcd(x, y), s"min($x, $y) != gcd($x, $y)")
  }

  assertMinEqualsGcd(conv_config.inputBusBitWidth, matmul_config.inputBusBitWidth)
  assertMinEqualsGcd(conv_config.kernelBusBitWidth, matmul_config.kernelBusBitWidth)
  assertMinEqualsGcd(conv_config.biasBusBitWidth, matmul_config.biasBusBitWidth)
  assertMinEqualsGcd(conv_config.requantizerBusBitWidth, matmul_config.requantizerBusBitWidth)
  assertMinEqualsGcd(conv_config.outputBusBitWidth, matmul_config.outputBusBitWidth)

  print(s"Configs:\n$conv_config\n$matmul_config")

  val conv_din_stream = slave(genStreamPort(conv_config.inputBusBitWidth bits, useLast = true))
  val conv_kernel_data_stream = slave(genStreamPort(conv_config.kernelBusBitWidth bits))
  val conv_requantizer_param_stream = slave(genStreamPort(conv_config.requantizerBusBitWidth bits))
  val conv_bias_data = slave(genStreamPort(conv_config.biasBusBitWidth bits))
  val conv_dout = master(genStreamPort(conv_config.outputBusBitWidth bits, useLast = true))
  val conv_line_width_sel =
    (conv_config.supportedInputWidths.length > 1) generate Bits(log2Up(conv_config.supportedInputWidths.length) bits)
  val conv_padding_data = (conv_config.maxPaddingSize > 0) generate SInt(conv_config.unitInDataBitWidth bits)
  val conv_padding_size = (conv_config.maxPaddingSize > 0) generate UInt(log2Up(conv_config.maxPaddingSize + 1) bits)

  val matmul_din_stream = slave(genStreamPort(matmul_config.inputBusBitWidth bits, useLast = true))
  val matmul_kernel_data_stream = slave(genStreamPort(matmul_config.kernelBusBitWidth bits))
  val matmul_requantizer_param_stream = slave(genStreamPort(matmul_config.requantizerBusBitWidth bits))
  val matmul_bias_data = slave(genStreamPort(matmul_config.biasBusBitWidth bits))
  val matmul_dout = master(genStreamPort(matmul_config.outputBusBitWidth bits, useLast = true))


  val conv_unit = ConvUnit(conv_config)
  val matmul_unit = MatMulUnit(matmul_config)

  conv_din_stream >> conv_unit.din_stream
  conv_kernel_data_stream >> conv_unit.kernel_data_stream
  conv_requantizer_param_stream >> conv_unit.requantizer_param_stream
  conv_bias_data >> conv_unit.bias_data
  conv_dout << conv_unit.dout
  if (conv_config.supportedInputWidths.length > 1) {
    in(conv_line_width_sel)
    conv_unit.line_width_sel := conv_line_width_sel
  }
  if (conv_config.maxPaddingSize > 0) {
    in(conv_padding_data)
    in(conv_padding_size)
    conv_unit.padding_data := conv_padding_data
    conv_unit.padding_size := conv_padding_size
  }

  matmul_din_stream >> matmul_unit.din_stream
  matmul_kernel_data_stream >> matmul_unit.kernel_data_stream
  matmul_requantizer_param_stream >> matmul_unit.requantizer_param_stream
  matmul_bias_data >> matmul_unit.bias_data
  matmul_dout << matmul_unit.dout

  XilinxBusTagger.tag(conv_din_stream, "conv_din")
  XilinxBusTagger.tag(conv_kernel_data_stream, "conv_kernel")
  XilinxBusTagger.tag(conv_requantizer_param_stream, "conv_requantizer")
  XilinxBusTagger.tag(conv_bias_data, "conv_bias")
  XilinxBusTagger.tag(conv_dout, "conv_dout")
  XilinxBusTagger.tag(matmul_din_stream, "matmul_din")
  XilinxBusTagger.tag(matmul_kernel_data_stream, "matmul_kernel")
  XilinxBusTagger.tag(matmul_requantizer_param_stream, "matmul_requantizer")
  XilinxBusTagger.tag(matmul_bias_data, "matmul_bias")
  XilinxBusTagger.tag(matmul_dout, "matmul_dout")

}
