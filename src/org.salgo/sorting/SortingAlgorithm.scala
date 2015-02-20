package org.salgo.sorting

import scala.annotation.tailrec
import scala.reflect.ClassTag

trait SortingAlgorithm {
  protected def swap[T](seq: Array[T], first: Int, second: Int) : Unit = {
    val tmp = seq(first)
    seq(first) = seq(second)
    seq(second) = tmp
  }

  @tailrec
  protected final def swap[T <: Any : ClassTag](seq: Seq[T], acc: Seq[T], replacement: T, valueToReplace: T)(implicit ev: T => Ordered[T]) : Seq[T] = seq match {
    case (h :: Nil) => if (h == valueToReplace) acc :+ replacement else acc :+ h
    case (h :: t) if h == valueToReplace => (acc :+ replacement) ++ t
    case (h1 :: h2 :: t) if h2 == valueToReplace => (acc :+ h1 :+ replacement) ++ t
    case (h :: t) => this.swap(t, acc ++ Seq(h), replacement, valueToReplace)
  }
}
