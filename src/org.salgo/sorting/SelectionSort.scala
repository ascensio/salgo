package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object SelectionSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    val n = seq.length - 1
    var current = 0
    while (current < n) {
      var min = current
      for (i <- current + 1 to n) {
        if (seq(i) < seq(min)) {
          min = i
        }
      }

      val minValue = seq(min)
      val currentValue = seq(current)
      seq.update(min, currentValue)
      seq.update(current, minValue)
      current += 1
    }
  }

  def sort[T <: Any : ClassTag](seq: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    this.sort(seq, Nil)
  }

  @tailrec
  private def sort[T <: Any : ClassTag](seq: Seq[T], result: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    seq match {
      case (h :: Nil) => result :+ h
      case (h :: t) =>
        val min = t.min
        if (min < h) this.sort(this.swap(t, Nil, h, t.min), result :+ min)
        else this.sort(t, result :+ h)
    }
  }

  @tailrec
  private def swap[T <: Any : ClassTag](seq: Seq[T], acc: Seq[T], replacement: T, valueToReplace: T)(implicit ev: T => Ordered[T]) : Seq[T] = seq match {
    case (h :: Nil) if h == valueToReplace => acc :+ replacement
    case (h :: Nil) => acc :+ h
    case (h :: t) if h == valueToReplace => (acc :+ replacement) ++ t
    case (h1 :: h2 :: t) if h2 == valueToReplace => (acc :+ h1 :+ replacement) ++ t
    case (h :: t) => this.swap(t, acc ++ Seq(h), replacement, valueToReplace)
  }
}
