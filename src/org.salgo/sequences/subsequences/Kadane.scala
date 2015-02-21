package org.salgo.sequences.subsequences

import scala.reflect.ClassTag

object Kadane {
  def findMaximumDoubleSubArray(array: Traversable[Double]) : (Double, Traversable[Double]) = {
    this.findMaximumSubArray[Double](array, 0.0, _ + _)
  }

  def findMaximumIntSubArray(array: Traversable[Int]) : (Int, Traversable[Int]) = {
    this.findMaximumSubArray[Int](array, 0, _ + _)
  }

  def findMaximumSubArray[T <: Any : ClassTag](array: Traversable[T], default: T, combine: (T, T) => T)(implicit ev: T => Ordered[T]) : (T, Traversable[T]) = {
    val finalMax = default
    val currentMax = default
    val currentSeq = Seq[T]()
    val finalSeq = Seq[T]()
    val result = array.foldLeft((currentMax, currentSeq, finalMax, finalSeq))((p: (T, Seq[T], T, Seq[T]), t) => combine(p._1, t) match {
      case c if c > default && c > p._3 => (c, p._2 :+ t, c, p._2 :+ t)
      case c if c > default => (c, p._2 :+ t, p._3, p._4)
      case c => (default, Seq[T](), p._3, p._4)
    })

    (result._3, result._4)
  }
}
