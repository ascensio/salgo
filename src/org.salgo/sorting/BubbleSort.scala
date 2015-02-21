package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

object BubbleSort extends GeneralSortingAlgorithm with GeneralFunctionalSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    var swapped = true
    var j = 0
    while (swapped) {
      swapped = false
      j += 1
      for (i <- 0 to seq.length - j - 1 by 1) {
        if (seq(i) > seq(i + 1)) {
          this.swap(seq, i, i + 1)
          swapped = true
        }
      }
    }
  }

  override def sort[T <: Any : ClassTag](seq: Seq[T])(implicit ev: (T) => Ordered[T]): Seq[T] = {
    this.sort(seq, Nil, Nil, continue = false)
  }

  @tailrec
  private def sort[T <: Any : ClassTag](seq: Seq[T], acc: Seq[T], finalAcc: Seq[T], continue: Boolean)(implicit ev: T => Ordered[T]) : Seq[T] = seq match {
    case Nil => finalAcc
    case (h :: Nil) if !continue => acc ++ (h +: finalAcc)
    case (h :: Nil) if continue => this.sort(acc, Nil, h +: finalAcc, continue = false)
    case (h1 :: h2 :: t) if h1 > h2 =>  this.sort(h1 :: t, acc :+ h2, finalAcc, continue = true)
    case (h1 :: h2 :: t) => this.sort(h2 :: t, acc :+ h1, finalAcc, continue)
  }
}
