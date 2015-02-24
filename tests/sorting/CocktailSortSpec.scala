package sorting

import org.salgo.sorting.{CocktailSort, GeneralFunctionalSortingAlgorithm}

class CocktailSortSpec extends GenericFunctionalSortSpec {
  override def getSortingAlgorithm: GeneralFunctionalSortingAlgorithm = CocktailSort
}
