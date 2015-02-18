package org.salgo.sorting

import scala.reflect.ClassTag

object MergeSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    val result = this.mergeSortCore[T](seq)
    for (i <- 0 to seq.length - 1) {
      seq.update(i, result(i))
    }
  }

  def mergeSortCore[T: ClassTag](a: Array[T])(implicit ev: T => Ordered[T]) : Array[T] = {
    a.length match {
      case 0 => a
      case 1 => a
      case _ =>
        val split = this.splitArray(a)
        this.merge[T](mergeSortCore(split._1), mergeSortCore(split._2))
    }
  }

  private def splitArray[T: ClassTag](a: Array[T]) : (Array[T], Array[T]) = {
    val half: Int = a.length / 2
    val left = a.slice(0, half)
    val right = a.slice(half, a.length)
    (left, right)
  }

  private def merge[T: ClassTag](left: Array[T], right: Array[T])(implicit ev: T => Ordered[T]) : Array[T] = {
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
}
