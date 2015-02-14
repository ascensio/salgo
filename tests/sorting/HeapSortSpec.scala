package sorting

import org.scalaalgo.sorting.{HeapSort, SortingAlgorithm}

class HeapSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = HeapSort
}
