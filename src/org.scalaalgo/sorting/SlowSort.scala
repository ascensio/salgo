package org.scalaalgo.sorting

import scala.reflect.ClassTag

object SlowSort extends GeneralSortingAlgorithm{
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    def coreSort(start: Int, n: Int) : Unit = {
      if (start >= n) return
      val m = (start + n) / 2
      coreSort(start, m)
      coreSort(m + 1, n)

      if (seq(n) < seq(m)) this.swap(seq, n, m)
      coreSort(start, n - 1)
    }

    if (seq.nonEmpty){
      coreSort(0, seq.length - 1)
    }
  }
}


