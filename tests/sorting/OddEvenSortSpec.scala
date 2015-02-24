package sorting

import org.salgo.sorting.{GeneralFunctionalSortingAlgorithm, OddEvenSort}

class OddEvenSortSpec extends GenericFunctionalSortSpec {
  override def getSortingAlgorithm: GeneralFunctionalSortingAlgorithm = OddEvenSort
}
