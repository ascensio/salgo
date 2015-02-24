package sorting

import org.salgo.sorting.{GeneralFunctionalSortingAlgorithm, InsertionSort}

class InsertionSortSpec extends GenericFunctionalSortSpec {
  override def getSortingAlgorithm: GeneralFunctionalSortingAlgorithm = InsertionSort
}