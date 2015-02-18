package org.salgo.sorting

import scala.reflect.ClassTag

object BubbleSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    var swapped = true
    var j = 0
    while (swapped) {
      swapped = false
      j += 1
      for (i <- 0 to seq.length - j - 1 by 1) {
        if (seq(i) > seq(i + 1)) {
          this.swap(seq, i, i + 1)
          swapped = true
        }
      }
    }
  }
}
