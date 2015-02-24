package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object GnomeSort extends GeneralFunctionalSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Seq[T])(implicit ev: T => Ordered[T]): Seq[T] = {
    this.sort(seq, Nil)
  }

  @tailrec
  private def sort[T <: Any : ClassTag](seq: Seq[T], acc: Seq[T])(implicit ev: T => Ordered[T]): Seq[T] = seq match {
    case Nil => Nil
    case (h :: Nil) => acc :+ h
    case (h1 :: h2 :: t) if h2 >= h1 => this.sort(h2 :: t, acc :+ h1)
    case (h1 :: h2 :: t) if h2 < h1 =>
      val newAcc = this.insert(acc.reverse, Nil, h2).reverse
      this.sort(h1 :: t, newAcc)
  }

  @tailrec
  private def insert[T <: Any : ClassTag](seq: Seq[T], acc: Seq[T], element: T)(implicit ev: T => Ordered[T]): Seq[T] = seq match {
    case Nil => acc :+ element
    case (h :: Nil) => if (h > element) acc :+ h :+ element  else acc :+ element :+ h
    case (h :: t) if h > element => this.insert(t, acc :+ h, element)
    case (h :: t) if h <= element => (acc :+ element) ++ seq
  }
}
