package test

trait TestTrait {
  def toBigInt(bitWidth: Int, dat: Array[Int]): BigInt =
    dat.foldRight(BigInt(0))((x, ret) => (ret << bitWidth) | (x & ((1 << bitWidth) - 1)))

  def fromBigInt(bitWidth: Int, dat: BigInt): Array[Int] = {
    assert(bitWidth <= 32)
    (0 until ((dat.bitLength + bitWidth - 1) / bitWidth))
      .map(i => {
        val masked = ((dat >> (i * bitWidth)) & ((1 << bitWidth) - 1)).toInt
        if  (masked > (1 << (bitWidth - 1)))
          masked - (1 << bitWidth)
        else
          masked
      })
      .toArray
  }
}
