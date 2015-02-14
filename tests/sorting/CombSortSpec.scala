package sorting

import org.scalaalgo.sorting.{CombSort, SortingAlgorithm}

class CombSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = CombSort
}
