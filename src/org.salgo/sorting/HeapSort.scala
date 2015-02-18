package org.salgo.sorting

import scala.reflect.ClassTag

object HeapSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    this.heapify(seq, seq.length)
    var end = seq.length - 1
    while (end > 0) {
      this.swap(seq, 0, end)
      end -= 1
      this.siftDown(seq, 0, end)
    }
  }

  private def heapify[T <: Any : ClassTag](seq: Array[T], count: Int)(implicit ev: T => Ordered[T]) = {
    var start = (count - 2) / 2

    while ( start >= 0) {
      this.siftDown(seq, start, count - 1)
      start -= 1
    }
  }

  private def siftDown[T <: Any : ClassTag](seq: Array[T], start: Int, end: Int)(implicit ev: T => Ordered[T]) : Unit = {
    var root = start
    var stopped = false
    while (root * 2 + 1 <= end && !stopped) {
      val child = root * 2 + 1
      var swap = root

      if (seq(swap) < seq(child)) swap = child
      if (child + 1 <= end && seq(swap) < seq(child + 1)) swap = child + 1
      if (swap != root) {
        this.swap(seq, root, swap)
        root = swap
      }
      else {
        stopped = true
      }
    }
  }
}
