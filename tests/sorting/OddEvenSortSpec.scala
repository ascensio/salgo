package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, OddEvenSort}

class OddEvenSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = OddEvenSort
}
