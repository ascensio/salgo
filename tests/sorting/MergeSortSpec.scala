package sorting

import org.scalaalgo.sorting.{GeneralSortingAlgorithm, MergeSort}

class MergeSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = MergeSort
 }
