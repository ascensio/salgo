package sorting

import org.scalaalgo.sorting.{IntegerSortingAlgorithm, RadixSort}

class RadixSortSpec extends IntegerSortSpec {
  override def getSortingAlgorithm: IntegerSortingAlgorithm= RadixSort
}
