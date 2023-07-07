package lib.utils

import lib.StreamController.StreamController
import spinal.core._

import scala.collection.mutable.ArrayBuffer

case class TreeReduce[T](
    // Result data of this TreeReduce operation
    // 本次TreeReduce的结果
    result: T,
    // Distance from the source, i.e. data in the seq
    // 距原始数据，即seq列表元素的距离
    distance: Int
)

object TreeReduce {
  def apply[T](
      seq: Seq[T],
      function: (TreeReduce[T], TreeReduce[T]) => TreeReduce[T],
      initialDistance: Int = 0
  ): TreeReduce[T] = {
    assert(seq.nonEmpty)
    if (seq.length == 1) return TreeReduce(seq.head, initialDistance)
    if (seq.length == 2) return function(TreeReduce(seq(0), initialDistance), TreeReduce(seq(1), initialDistance))
    val mid = seq.length / 2
    function(
      apply(seq.slice(0, mid), function),
      apply(seq.slice(mid, seq.length), function)
    )
  }

  def simple[T](seq: Seq[T], function: (T, T) => T): T = {
    val translated_function = (a: TreeReduce[T], b: TreeReduce[T]) => TreeReduce(function(a.result, b.result), 0)
    apply(seq, translated_function).result
  }

  def distance[T](seq: Seq[T]): Int = {
    apply(seq, (a: TreeReduce[T], b: TreeReduce[T]) => TreeReduce(a.result, a.distance.max(b.distance) + 1)).distance
  }

  def withRegs[T <: Data](
      seq: Seq[T],
      register_distance: Int,
      function: (T, T) => T,
      reg_at_end: Boolean = true,
      name_prefix: String = "tree"
  ) = new Area {
    private val max_distance = distance(seq)
    private val stream_depth = (max_distance - 1) / register_distance + 1

    val controller = StreamController(stream_depth)

    private val countRegInCurrentLevel = Array.fill(stream_depth)(0)
    private val countResultInCurrentLayer = Array.fill(max_distance)(0)

    private def RegDataAt(data: T, depth: Int) = {
      val reg = RegNextWhen(data, controller.en(depth), data.getZero)
      reg.setName((name_prefix + "_r" + depth) + countRegInCurrentLevel(depth))
      countRegInCurrentLevel(depth) += 1
      reg
    }

    private val build_tree = (a: TreeReduce[T], b: TreeReduce[T]) => {
      var first = if (a.distance <= b.distance) a else b
      val second = if (first == a) b else a

      val second_reg_count = second.distance / register_distance
      var first_reg_count = first.distance / register_distance
      while (second_reg_count - first_reg_count > 0) {
        first = TreeReduce(
          RegDataAt(first.result, first_reg_count),
          (first_reg_count + 1) * register_distance
        )
        first_reg_count = first.distance / register_distance
      }

      val result = function(first.result, second.result)
      val result_distance = second.distance + 1
      result.setName((name_prefix + "_f" + result_distance) + countResultInCurrentLayer(result_distance - 1))
      countResultInCurrentLayer(result_distance - 1) += 1

      val result_reg_count = result_distance / register_distance
      val reminder = result_distance % register_distance

      val final_result = if (reminder == 0) RegDataAt(result, result_reg_count - 1) else result

      TreeReduce(final_result, result_distance)
    }

    private val output = apply(seq, build_tree, 0)

    val result =
      if (reg_at_end && output.distance % register_distance != 0)
        RegDataAt(output.result, output.distance / register_distance)
      else
        output.result
  }
}
