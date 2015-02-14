package sorting

import org.scalaalgo.sorting.{GeneralSortingAlgorithm, InsertionSort}

class InsertionSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = InsertionSort
}