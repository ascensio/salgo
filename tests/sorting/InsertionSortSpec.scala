package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, InsertionSort}

class InsertionSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = InsertionSort
}