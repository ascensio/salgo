package org.sortex

object SelectionSort extends SortingAlgorithm {
  def sort[T](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    val n = seq.length - 1
    var current = 0
    while (current < n) {
      var min = current
      for (i <- current + 1 to n) {
        if (seq(i) < seq(min)) {
          min = i
        }
      }

      val minValue = seq(min)
      val currentValue = seq(current)
      seq.update(min, currentValue)
      seq.update(current, minValue)
      current += 1
    }
  }
}
