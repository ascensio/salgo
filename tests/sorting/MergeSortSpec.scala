package sorting

import org.salgo.sorting.{GeneralFunctionalSortingAlgorithm, MergeSort}

class MergeSortSpec extends GenericFunctionalSortSpec {
  override def getSortingAlgorithm: GeneralFunctionalSortingAlgorithm = MergeSort
 }
