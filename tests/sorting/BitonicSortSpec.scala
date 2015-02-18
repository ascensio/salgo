package sorting

import org.salgo.sorting.{BitonicSort, GeneralSortingAlgorithm}

class BitonicSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = BitonicSort
}
