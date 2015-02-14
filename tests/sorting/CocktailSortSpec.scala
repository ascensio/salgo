package sorting

import org.scalaalgo.sorting.{CocktailSort, SortingAlgorithm}

class CocktailSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: SortingAlgorithm = CocktailSort
}
