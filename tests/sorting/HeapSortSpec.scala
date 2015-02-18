package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, HeapSort}

class HeapSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = HeapSort
}
