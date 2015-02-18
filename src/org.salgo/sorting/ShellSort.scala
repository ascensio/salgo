package org.salgo.sorting

import scala.reflect.ClassTag

object ShellSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    val gaps = List(701, 301, 132, 57, 23, 10, 4 ,1)
    this.sort(seq, gaps)
  }

  def sort[T <: Any : ClassTag](seq: Array[T], gaps: Traversable[Int])(implicit ev: T => Ordered[T]) : Unit = {
    for (gap <- gaps) {
      for (i <- gap to seq.length - 1) {
        val tmp = seq(i)
        var j = i
        while (j >= gap && seq(j - gap) > tmp) {
          seq(j) = seq(j - gap)
          j -= gap
        }
        seq(j) = tmp
      }
    }
  }
}
