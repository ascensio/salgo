package sorting

import org.scalaalgo.sorting.{OddEvenSort, SortingAlgorithm}

class OddEvenSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = OddEvenSort
}
