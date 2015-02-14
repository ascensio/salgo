package sorting

import org.scalaalgo.sorting.{GeneralSortingAlgorithm, OddEvenSort}

class OddEvenSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = OddEvenSort
}
