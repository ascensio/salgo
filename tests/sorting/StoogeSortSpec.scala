package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, StoogeSort}

class StoogeSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = StoogeSort
}
