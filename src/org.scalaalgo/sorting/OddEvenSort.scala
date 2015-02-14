package org.scalaalgo.sorting

import scala.reflect.ClassTag

object OddEvenSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    val length = seq.length - 1
    var sorted = false

    while (!sorted) {
      sorted = true
      for (i <- 1 to length by 2 if i + 1 <= length) {
        if (seq(i) > seq(i + 1)) {
          this.swap(seq, i, i + 1)
          sorted = false
        }
      }

      for (i <- 0 to length by 2 if i + 1 <= length) {
        if (seq(i) > seq(i + 1)) {
          this.swap(seq, i, i + 1)
          sorted = false
        }
      }
    }
  }
}
