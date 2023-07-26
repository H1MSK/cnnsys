package lib.utils

import spinal.core._
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4Stream

object XilinxBusTagger {
  def tag(axis: Axi4Stream, interface_name: String) = {
    def set[T <: SpinalTagReady](port: T, name: String) =
      port.addAttribute("X_INTERFACE_INFO", "xilinx.com:interface:axis:1.0 " + interface_name + " " + name)
    set(axis.valid, "TVALID")
    set(axis.ready, "TREADY")
    set(axis.data, "TDATA")
    if (axis.config.useId)
      set(axis.id, "TID")
    if (axis.config.useDest)
      set(axis.dest, "TDEST")
    if (axis.config.useStrb)
      set(axis.strb, "TSTRB")
    if (axis.config.useKeep)
      set(axis.keep, "TKEEP")
    if (axis.config.useLast)
      set(axis.last, "TLAST")
    if (axis.config.useUser)
      set(axis.user, "TUSER")
  }
}
