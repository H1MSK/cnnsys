package lib.quantizer

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class RequantizerChain(
    chain_length: Int,
    enableChainOut: Boolean,
    parallel_count: Int,
    config: RequantizerConfig
) extends Component {
  val din = slave Stream Vec(Vec(SInt(config.din_bitwidth bits), parallel_count), chain_length)
  val dout = master Stream Vec(Vec(SInt(config.dout_bitwidth bits), parallel_count), chain_length)
  val param = slave Flow RequantizerParamBundle(config)

  val chain = Array.fill(chain_length)(Requantizer(parallel_count, config))

  val out_param = enableChainOut generate (master Flow RequantizerParamBundle(config))

  private val reg_config: RequantizerParamBundle =
    RegNextWhen(param.payload, param.valid, init = param.payload.getZero)
  val history_config =
    History(reg_config, length = chain_length, when = param.valid, init = reg_config.getZero).reverse

  din.ready := chain.head.din.ready
  dout.valid := chain.head.dout.valid
  chain.indices.foreach(i => {
    val rq = chain(i)
    rq.din.valid := din.valid
    rq.din.payload := din.payload(i)
    rq.dout.ready := dout.ready
    rq.param := history_config(i)
    dout.payload(i) := rq.dout.payload
  })

  if (enableChainOut) {
    out_param.payload := history_config.last
    out_param.valid := param.valid
  }
  def apply(x: Int) = chain(x)
}
