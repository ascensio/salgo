package org.salgo.sorting

import scala.reflect.ClassTag

object CycleSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    val length = seq.length
    for (start <- 0 to length - 2) {
      var currentItem = seq(start)

      var pos = start
      for (i <- start + 1 to length - 1) {
        if (seq(i) < currentItem) pos += 1
      }

      if (pos != start) {
        while (seq(pos) == currentItem) pos += 1
        val nextItem = seq(pos)
        seq(pos) = currentItem
        currentItem = nextItem

        this.swap(seq, pos, start)

        while (pos != start) {
          pos = start
          for (i <- start + 1 to length - 1) {
            if (seq(i) < currentItem) pos += 1
          }

          while (seq(pos) == currentItem) pos += 1
          val nextItem = seq(pos)
          seq(pos) = currentItem
          currentItem = nextItem
        }
      }
    }
  }
}
