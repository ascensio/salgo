package org.scalaalgo.sorting

import scala.reflect.ClassTag

object SwapSort extends SortingAlgorithm{
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    var start = 0
    val n = seq.length - 1
    while (start < n) {
      val currentValue = seq(start)
      val smallerValueCount = seq.view(start + 1, n + 1).count(e => e < currentValue)
      smallerValueCount match {
        case i if i > 0 => this.swap(seq, start, start + smallerValueCount)
        case _ => start += 1
      }
    }
  }
}




