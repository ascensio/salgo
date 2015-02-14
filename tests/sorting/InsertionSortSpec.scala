package sorting

import org.scalaalgo.sorting.{InsertionSort, SortingAlgorithm}

class InsertionSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = InsertionSort
}