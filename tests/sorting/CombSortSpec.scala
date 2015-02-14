package sorting

import org.scalaalgo.sorting.{CombSort, GeneralSortingAlgorithm}

class CombSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = CombSort
}
