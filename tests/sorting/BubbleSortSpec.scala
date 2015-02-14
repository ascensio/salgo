package sorting

import org.scalaalgo.sorting.{BubbleSort, SortingAlgorithm}

class BubbleSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = BubbleSort
}
