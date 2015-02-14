package org.scalaalgo.sorting

trait IntegerSortingAlgorithm extends SortingAlgorithm {
  def sortCopy(seq: Traversable[Int])(implicit ev: Int => Ordered[Int]) : Array[Int] = {
    val copiedSeq = seq.toArray
    this.sort(copiedSeq)
    copiedSeq
  }

  def sort(seq: Array[Int])(implicit ev: Int => Ordered[Int]) : Unit
}
