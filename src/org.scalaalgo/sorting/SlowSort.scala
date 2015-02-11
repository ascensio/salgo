package org.scalaalgo.sorting

object SlowSort extends SortingAlgorithm{
  def sort[T](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    def coreSort[T](start: Int, n: Int) : Unit = {
      if (start >= n) return
      val m = (start + n) / 2
      coreSort(start, m)
      coreSort(m + 1, n)

      if (seq(n) < seq(m)) this.swap(seq, n, m)
      coreSort(start, n - 1)
    }

    if (!seq.isEmpty){
      coreSort(0, seq.length - 1)
    }
  }
}


