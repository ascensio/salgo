package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object OddEvenSort extends GeneralFunctionalSortingAlgorithm {
  override def sort[T <: Any : ClassTag](seq: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    this.sortCore(seq, Seq[T](), isOdd = true, isChanged = false)
  }

  @tailrec
  private def sortCore[T<: Any : ClassTag](seq: Seq[T], acc: Seq[T], isOdd: Boolean, isChanged: Boolean)(implicit ev: T => Ordered[T]) : Seq[T] = {
    seq match {
      case Nil if isOdd =>
        if (acc.isEmpty) acc
        else this.sortCore(acc.tail, Seq[T](acc.head), !isOdd, isChanged = false)
      case Nil =>
        if (isChanged) this.sortCore(acc, Seq[T](), !isOdd, isChanged)
        else acc
      case (h :: Nil) =>
        if (isOdd) {
          if (acc.isEmpty) acc :+ h
          else this.sortCore((acc :+ h).tail, Seq[T](acc.head), !isOdd, isChanged = false)
        } else {
          if (isChanged) this.sortCore(acc :+ h, Seq[T](), !isOdd, isChanged)
          else acc :+ h
        }
      case (h1 :: h2 :: t) =>
        if (h1 > h2) this.sortCore(t, acc :+ h2 :+ h1, isOdd, isChanged = true)
        else this.sortCore (t, acc :+ h1 :+ h2, isOdd, isChanged)
    }
  }
}
