package org.sortex

import scala.reflect.ClassTag

trait SortingAlgorithm {
  def sortCopy[T <: Any : ClassTag](seq: Traversable[T])(implicit ev: T => Ordered[T]) : Array[T] = {
    val copiedSeq = seq.toArray
    this.sort(copiedSeq)
    copiedSeq
  }

  def sort[T](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit

  def swap[T](seq: Array[T], first: Int, second: Int) : Unit = {
    val tmp = seq(first)
    seq(first) = seq(second)
    seq(second) = tmp
  }
}
