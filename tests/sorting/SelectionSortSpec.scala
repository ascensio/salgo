package sorting

import org.scalaalgo.sorting.{GeneralSortingAlgorithm, SelectionSort}

class SelectionSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = SelectionSort
 }
