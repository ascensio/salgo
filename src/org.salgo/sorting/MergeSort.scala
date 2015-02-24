package org.salgo.sorting

import scala.annotation.tailrec
import scala.collection.immutable.::
import scala.reflect.ClassTag

object MergeSort extends GeneralFunctionalSortingAlgorithm {
  def sort[T: ClassTag](a: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    a.length match {
      case n if n < 2 => a
      case _ =>
        val (left, right) = a splitAt (a.length / 2)
        this.mergeRecursive[T](sort(left), sort(right), Nil)
    }
  }

  @tailrec
  private def mergeRecursive[T: ClassTag](left: Seq[T], right: Seq[T], result: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = (left, right) match {
    case (Nil, Nil) => result
    case (_, Nil) =>result ++ this.sort(left)
    case (Nil, _) => result ++ this.sort(right)
    case (lh :: lt, rh :: rt) if lh < rh => this.mergeRecursive[T](lt, right, result ++ List(lh))
    case (lh :: lt, rh :: rt) => this.mergeRecursive(left, rt, result ++ List(rh))
  }
}

