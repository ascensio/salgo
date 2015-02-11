package org.scalaalgo.sorting

object StoogeSort extends SortingAlgorithm{
  def sort[T](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    def coreSort[T](start: Int, n: Int) : Unit = {
      if (seq(start) > seq(n - 1)) {
        this.swap(seq, start, n - 1)
      }

      val len = n - start
      if (len > 2) {
        val k = len / 3
        coreSort(start, n - k)
        coreSort(start + k, n)
        coreSort(start, n - k)
      }
    }

    if (!seq.isEmpty){
      coreSort(0, seq.length)
    }
  }
}
