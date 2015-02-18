package org.salgo.sorting

import scala.reflect.ClassTag

object StoogeSort extends GeneralSortingAlgorithm{
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    def coreSort(start: Int, n: Int) : Unit = {
      if (seq(start) > seq(n - 1)) {
        this.swap(seq, start, n - 1)
      }

      val len = n - start
      if (len > 2) {
        val k = len / 3
        coreSort(start, n - k)
        coreSort(start + k, n)
        coreSort(start, n - k)
      }
    }

    if (seq.nonEmpty){
      coreSort(0, seq.length)
    }
  }
}
