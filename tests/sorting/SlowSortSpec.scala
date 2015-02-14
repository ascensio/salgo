package sorting

import org.scalaalgo.sorting.{SlowSort, SortingAlgorithm}

class SlowSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = SlowSort
 }
