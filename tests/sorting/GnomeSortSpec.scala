package sorting

import org.scalaalgo.sorting.{GnomeSort, SortingAlgorithm}

class GnomeSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = GnomeSort
}
