package org.salgo.sorting

import scala.reflect.ClassTag

trait GeneralSortingAlgorithm extends SortingAlgorithm {
  def sortCopy[T <: Any : ClassTag](seq: Traversable[T])(implicit ev: T => Ordered[T]) : Traversable[T] = {
    val copiedSeq = seq.toArray
    this.sort(copiedSeq)
    copiedSeq
  }

  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit
}





