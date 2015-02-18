package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, GnomeSort}

class GnomeSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = GnomeSort
}
