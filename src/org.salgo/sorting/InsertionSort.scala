package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object InsertionSort extends GeneralFunctionalSortingAlgorithm {
  override def sort[T <: Any : ClassTag](seq: Seq[T])(implicit converter: T => Ordered[T]) : Seq[T] = {
    this.sort(seq, Nil)
  }

  @tailrec
  def sort[T <: Any : ClassTag](seq: Seq[T], acc: Seq[T])(implicit converter: T => Ordered[T]) : Seq[T] = seq match {
    case Nil => Nil
    case (h :: Nil) => this.insertElement(acc.reverse, Nil, h).reverse
    case (h1 :: t) => this.sort(t, this.insertElement(acc.reverse, Nil, h1).reverse)
  }

  @tailrec
  private def insertElement[T <: Any : ClassTag](seq: Seq[T], acc: Seq[T], element: T)(implicit converter: T => Ordered[T]) : Seq[T] = seq match {
    case Nil => acc :+ element
    case (h :: Nil) => if (h > element) acc :+ h :+ element  else acc :+ element :+ h
    case (h :: t) if h > element => this.insertElement(t, acc :+ h, element)
    case (h :: t) if h <= element => (acc :+ element) ++ seq
  }
}



