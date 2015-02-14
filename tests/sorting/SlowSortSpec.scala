package sorting

import org.scalaalgo.sorting.{GeneralSortingAlgorithm, SlowSort}

class SlowSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = SlowSort
 }
