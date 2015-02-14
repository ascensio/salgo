package sorting

import org.scalaalgo.sorting.{CocktailSort, GeneralSortingAlgorithm}

class CocktailSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = CocktailSort
}
