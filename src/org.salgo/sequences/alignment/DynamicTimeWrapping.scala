package org.salgo.sequences.alignment

import org.salgo.common.MathUtils

object DynamicTimeWrapping {
  def run[T](first: IndexedSeq[T], second: IndexedSeq[T], distance: (T, T) => Int) : Int = {
    val matrix = Array.fill[Array[Int]](first.length)(Array.fill[Int](second.length)(Int.MaxValue))
    matrix(0)(0) = 0

    for (i <- 1 to first.length - 1; j <- 1 to second.length - 1) {
      val cost = distance(first(i), second(j))
      val min = MathUtils.min(matrix(i-1)(j), matrix(i)(j-1), matrix(i-1)(j-1)).getOrElse(0)
      matrix(i)(j) = cost + min
    }

    matrix(first.length - 1)(second.length - 1)
  }
}
