package sorting

import org.salgo.sorting.{GeneralFunctionalSortingAlgorithm, GnomeSort}

class GnomeSortSpec extends GenericFunctionalSortSpec {
  override def getSortingAlgorithm: GeneralFunctionalSortingAlgorithm = GnomeSort
}
