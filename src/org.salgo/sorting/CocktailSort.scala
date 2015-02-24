package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object CocktailSort extends GeneralFunctionalSortingAlgorithm{
  def sort[T <: Any : ClassTag](seq: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T] = {
    this.sortCore(seq, Seq[T](), Seq[T](), Seq[T](), sortingUp = true, hasChanged = false)
  }

  @tailrec
  def sortCore[T <: Any : ClassTag](seq: Seq[T], acc: Seq[T], resultStart: Seq[T], resultEnd: Seq[T], sortingUp: Boolean, hasChanged: Boolean)(implicit ev: T => Ordered[T]) : Seq[T]= seq match {
    case Nil => Nil
    case (h :: Nil) if sortingUp =>
      if (hasChanged) this.sortCore(acc.reverse, Seq[T](), resultStart, h +: resultEnd, sortingUp = false, hasChanged = false)
      else resultStart ++ (acc :+ h) ++ resultEnd
    case (h :: Nil) =>
      if (hasChanged) this.sortCore(acc.reverse, Seq[T](), resultStart :+ h, resultEnd, sortingUp = true, hasChanged = false)
      else resultStart ++ (h +: acc) ++ resultEnd
    case (h1 :: h2 :: t)  =>
      if (sortingUp && h1 > h2) this.sortCore(h1 :: t, acc :+ h2, resultStart, resultEnd, sortingUp, hasChanged = true)
      else if (!sortingUp && h1 < h2) this.sortCore(h1 :: t, acc :+ h2, resultStart, resultEnd, sortingUp, hasChanged = true)
      else this.sortCore(h2 :: t, acc :+ h1, resultStart, resultEnd, sortingUp, hasChanged)
  }
}