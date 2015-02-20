package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object GnomeSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]): Unit = {
    var position = 1
    var last = 0
    val length = seq.length
    while (position < length) {
      if (seq(position) >= seq(position - 1)) {
        if (last != 0) {
          position = last
          last = 0
        }
        position += 1
      }
      else {
        this.swap(seq, position, position - 1)
        if (position > 1) {
          if (last == 0) last = position
          position -= 1
        }
        else {
          position += 1
        }
      }
    }
  }

  def sort[T <: Any : ClassTag](seq: Seq[T])(implicit ev: T => Ordered[T]): Seq[T] = {
    this.sort(seq, Nil)
  }

  @tailrec
  private def sort[T <: Any : ClassTag](seq: Seq[T], acc: Seq[T])(implicit ev: T => Ordered[T]): Seq[T] = seq match {
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
