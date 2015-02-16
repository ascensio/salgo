package sorting

import org.scalaalgo.sorting.{BitonicSort, GeneralSortingAlgorithm}

class BitonicSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = BitonicSort
}
