package sorting

import org.salgo.sorting.{IntegerSortingAlgorithm, RadixSort}

class RadixSortSpec extends IntegerSortSpec {
  override def getSortingAlgorithm: IntegerSortingAlgorithm= RadixSort
}
