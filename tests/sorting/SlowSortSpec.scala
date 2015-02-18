package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, SlowSort}

class SlowSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = SlowSort
 }
