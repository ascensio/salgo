package sorting

import org.salgo.sorting.IntegerSortingAlgorithm
import org.scalatest.{FunSuite, Matchers}

abstract class IntegerSortSpec extends FunSuite with Matchers {
  test("Sort unsorted array") { this.getSortingAlgorithm.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual expectedShort }
  test("Sort empty array") { this.getSortingAlgorithm.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sort already sorted array") { this.getSortingAlgorithm.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual expectedShort }
  test("Sort single element array") { this.getSortingAlgorithm.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Sort inverse ordered array") { this.getSortingAlgorithm.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual expectedShort }
  test("Sort array with duplicate elements") { this.getSortingAlgorithm.sortCopy(Array(4, 5, 3, 3, 5)) shouldEqual Array(3, 3, 4, 5, 5) }
  test("Sort array with single value") { this.getSortingAlgorithm.sortCopy(Array(4, 4, 4, 4, 4)) shouldEqual Array(4, 4, 4, 4, 4) }
  test("Sort long array") { this.getSortingAlgorithm.sortCopy(Array(6, 1, 0, 3, 3, 5, 7, 2, 1, 6, 5, 4 ,3 ,1, 2, 8, 8, 9 ,7)) shouldEqual Array(0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9) }

  def getSortingAlgorithm: IntegerSortingAlgorithm

  private def expectedShort = Array(1, 2, 3, 4, 5)
}
