package sorting

import org.scalaalgo.sorting.{SelectionSort, SortingAlgorithm}

class SelectionSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = SelectionSort
 }
