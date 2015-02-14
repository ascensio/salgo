package sorting

import org.scalaalgo.sorting.{StoogeSort, SortingAlgorithm}

class StoogeSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = StoogeSort
}
