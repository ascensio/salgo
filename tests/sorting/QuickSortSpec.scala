package sorting

import org.salgo.sorting.{GeneralFunctionalSortingAlgorithm, QuickSort}

class QuickSortSpec extends GenericFunctionalSortSpec {
  override def getSortingAlgorithm: GeneralFunctionalSortingAlgorithm = QuickSort
 }
