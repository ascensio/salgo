package sorting

import org.scalaalgo.sorting.{GeneralSortingAlgorithm, GnomeSort}

class GnomeSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = GnomeSort
}
