package org.scalaalgo.sorting

trait SortingAlgorithm {
  protected def swap[T](seq: Array[T], first: Int, second: Int) : Unit = {
    val tmp = seq(first)
    seq(first) = seq(second)
    seq(second) = tmp
  }
}
