package sorting

import org.scalaalgo.sorting.{GeneralSortingAlgorithm, StoogeSort}

class StoogeSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = StoogeSort
}
