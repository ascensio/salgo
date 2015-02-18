package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object QuickSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    this.coreSort(seq, 0, seq.length - 1)
  }

  private def coreSort[T](a: Array[T], left: Int, right: Int)(implicit ev: T => Ordered[T]) : Unit = {
    if (a.length != 0 && left < right) {
      val newRange = this.coreSortRange(a, left, right)
      val newLeft = newRange._1
      val newRight = newRange._2
      if (left < newRight) this.coreSort(a, left, newRight)
      if (newLeft < right) this.coreSort(a, newLeft, right)
    }
  }

  def sortCopyTailRec[T <: Any : ClassTag](seq: Traversable[T])(implicit ev: T => Ordered[T]) : Array[T] = {
    val copiedSeq = seq.toArray
    this.sortTailRec(copiedSeq)
    copiedSeq
  }

  def sortTailRec[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    this.coreSortTail(seq, List[(Int, Int)]((0, seq.length - 1)))
  }

  @tailrec
  private def coreSortTail[T](a: Array[T], ranges: List[(Int, Int)])(implicit ev: T => Ordered[T]) : Unit = {
    if (a.length != 0){
      ranges.head match {
        case (left, right) if left < right =>
          var newRanges: List[(Int, Int)] = ranges.tail
          this.coreSortRange(a, left, right) match {
            case (l, r) =>
              if (left < r) newRanges = (left, r) :: newRanges
              if (l < right) newRanges = (l, right) :: newRanges
          }
          if (newRanges.nonEmpty) this.coreSortTail(a, newRanges)
        case _ =>
      }
    }
  }

  private def coreSortRange[T](a: Array[T], left: Int, right: Int)(implicit ev: T => Ordered[T]) : (Int, Int) = {
    val middleIndex = (left + right) / 2
    val pivot = a(middleIndex)
    var l = left
    var r = right

    while (l <= r) {
      while(a(l) < pivot) { l += 1 }
      while(a(r) > pivot) { r -= 1 }
      if (l <= r) {
        this.swap(a, l, r)
        l += 1
        r -= 1
      }
    }

    (l, r)
  }
}
