package sorting

import org.salgo.sorting.{CycleSort, GeneralSortingAlgorithm}

class CycleSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = CycleSort
}
