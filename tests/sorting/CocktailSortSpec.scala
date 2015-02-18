package sorting

import org.salgo.sorting.{CocktailSort, GeneralSortingAlgorithm}

class CocktailSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = CocktailSort
}
