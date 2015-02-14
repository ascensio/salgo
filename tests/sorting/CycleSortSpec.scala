package sorting

import org.scalaalgo.sorting.{CycleSort, GeneralSortingAlgorithm}

class CycleSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = CycleSort
}
