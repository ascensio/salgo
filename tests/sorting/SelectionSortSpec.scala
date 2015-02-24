package sorting

import org.salgo.sorting.{GeneralFunctionalSortingAlgorithm, SelectionSort}

class SelectionSortSpec extends GenericFunctionalSortSpec {
  override def getSortingAlgorithm: GeneralFunctionalSortingAlgorithm = SelectionSort
}
