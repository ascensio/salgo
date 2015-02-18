package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, SelectionSort}

class SelectionSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = SelectionSort
 }
