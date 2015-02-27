package org.salgo.sorting

import scala.reflect.ClassTag

object QuickSort extends GeneralFunctionalSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    if (seq.length < 2) seq
    else {
      val pivotIndex = seq.length / 2
      val pivot = seq(pivotIndex)
      sort(seq.filter(_ < pivot)) ++ seq.filter(_ == pivot) ++ sort(seq.filter(_ > pivot))
    }
  }
}
