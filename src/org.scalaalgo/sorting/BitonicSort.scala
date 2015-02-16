package org.scalaalgo.sorting

import scala.reflect.ClassTag

object BitonicSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]) : Unit = {
    this.coreSort(seq, 0, seq.length, sortUp = true)
  }

  private def coreSort[T <: Any : ClassTag](seq: Array[T], low: Int, high: Int, sortUp: Boolean)(implicit ev: T => Ordered[T]) : Unit = {
    if (high > 1) {
      val middle = high / 2
      this.coreSort(seq, low, middle, !sortUp)
      this.coreSort(seq, low + middle, high - middle, sortUp)
      this.merge(seq, low, high, sortUp)
    }
  }

  private def merge[T <: Any : ClassTag](seq: Array[T], low: Int, high: Int, sortUp: Boolean)(implicit ev: T => Ordered[T]) : Unit = {
    if (high > 1) {
      val power = this.getBestMatchingPowerOfTwo(high)
      for (i <- low to low + high - power) {
        val compareIndex = i + power
        if (compareIndex < seq.length && sortUp == (seq(i) > seq(compareIndex))) this.swap(seq, i, compareIndex)
      }
      this.merge(seq, low, power, sortUp)
      this.merge(seq, low + power, high - power, sortUp)
    }
  }

  private def getBestMatchingPowerOfTwo(n: Int) : Int = {
    var k = 1
    while (k < n) k = k << 1
    k >> 1
  }
}
