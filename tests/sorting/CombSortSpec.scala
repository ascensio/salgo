package sorting

import org.salgo.sorting.{CombSort, GeneralSortingAlgorithm}

class CombSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = CombSort
}
