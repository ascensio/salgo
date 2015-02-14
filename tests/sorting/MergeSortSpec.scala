package sorting

import org.scalaalgo.sorting.{MergeSort, SortingAlgorithm}

class MergeSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = MergeSort
 }
