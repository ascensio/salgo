package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, InsertionSort}

class InsertionSortSpec extends GenericSortSpec {
  test("Sort long list 1") { InsertionSort.sort(List(6, 1, 0, 3, 3, 5, 7, 2, 1, 6, 5, 4 ,3 ,1, 2, 8, 8, 9 ,7)) shouldEqual List(0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9) }


  override def getSortingAlgorithm: GeneralSortingAlgorithm = InsertionSort
}