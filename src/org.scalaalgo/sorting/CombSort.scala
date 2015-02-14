package org.scalaalgo.sorting

import scala.reflect.ClassTag

object CombSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    var step = seq.length
    var exchanged = false
    while (step > 1 || exchanged) {
      exchanged = false
      if (step > 1) step = (step / 1.3).toInt
      for (i <- 0 to seq.length - step - 1) {
        val first = seq(i)
        val second = seq(i + step)
        if (first > second) {
          this.swap(seq, i, i + step)
          exchanged = true
        }
      }
    }
  }
}
