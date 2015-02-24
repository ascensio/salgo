package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object SelectionSort extends GeneralFunctionalSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    this.sort(seq, Nil)
  }

  @tailrec
  private def sort[T <: Any : ClassTag](seq: Seq[T], result: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = seq match {
    case Nil => Nil
    case (h :: Nil) => result :+ h
    case (h :: t) =>
      val min = t.min
      if (min < h) this.sort(this.swap(t, Nil, h, t.min), result :+ min)
      else this.sort(t, result :+ h)
  }
}
