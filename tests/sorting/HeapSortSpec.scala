package sorting

import org.scalaalgo.sorting.{GeneralSortingAlgorithm, HeapSort}

class HeapSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = HeapSort
}
