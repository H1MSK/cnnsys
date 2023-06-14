package utils

object TreeReduce {
  def apply[T](seq: Seq[T], function: (T, T)=>T): T = {
    assert(seq.nonEmpty)
    if (seq.length == 1) return seq(0)
    if (seq.length == 2) return function(seq(0), seq(1))
    val mid = seq.length / 2
    return function(apply(seq.slice(0, mid), function), apply(seq.slice(mid, seq.length), function))
  }
}
