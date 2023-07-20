package lib.quantizer

case class RequantizerConfig(
    din_bitwidth: Int,
    dout_bitwidth: Int,
    scale_bitwidth: Int,
    useOffset: Boolean = true,
    useRightShift: Boolean = true,
    useOffsetSaturation: Boolean = true
) {}
