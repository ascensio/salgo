package org.salgo.sorting

import scala.annotation.tailrec
import scala.collection.immutable.::
import scala.reflect.ClassTag

object MergeSort extends GeneralSortingAlgorithm  with GeneralFunctionalSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    val result = this.mergeSortCore[T](seq)
    for (i <- 0 to seq.length - 1) {
      seq.update(i, result(i))
    }
  }

  def mergeSortCore[T: ClassTag](a: Array[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    a.length match {
      case n if n < 2 => a
      case _ =>
        val (left, right) = a splitAt (a.length / 2)
        this.mergeIterative[T](mergeSortCore(left), mergeSortCore(right))
    }
  }

  def sort[T: ClassTag](a: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    a.length match {
      case n if n < 2 => a
      case _ =>
        val (left, right) = a splitAt (a.length / 2)
        this.mergeRecursive[T](sort(left), sort(right), Nil)
    }
  }

  private def mergeIterative[T: ClassTag](left: Seq[T], right: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    val result = new Array[T](left.length + right.length)
    var leftIndex = 0
    var rightIndex = 0
    var index = 0

    while (leftIndex < left.length && rightIndex < right.length) {
      val firstLeft = left(leftIndex)
      val firstRight = right(rightIndex)
      if (firstLeft <= firstRight) {
        result.update(index, firstLeft)
        index += 1
        leftIndex += 1
      }
      else {
        result.update(index, firstRight)
        index += 1
        rightIndex += 1
      }
    }

    while (leftIndex < left.length) {
      val firstLeft = left(leftIndex)
      result.update(index, firstLeft)
      index += 1
      leftIndex += 1
    }

    while (rightIndex < right.length) {
      val firstRight = right(rightIndex)
      result.update(index, firstRight)
      index += 1
      rightIndex += 1
    }

    result
  }

  @tailrec
  private def mergeRecursive[T: ClassTag](left: Seq[T], right: Seq[T], result: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = (left, right) match {
    case(Nil, Nil) => result
    case (_, Nil) =>result ++ this.sort(left)
    case (Nil, _) => result ++ this.sort(right)
    case (lh :: lt, rh :: rt) if lh < rh => this.mergeRecursive[T](lt, right, result ++ List(lh))
    case  (lh :: lt, rh :: rt) => this.mergeRecursive(left, rt, result ++ List(rh))
  }
}

