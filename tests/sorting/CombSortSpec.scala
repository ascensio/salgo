package sorting

import org.scalaalgo.sorting.CombSort
import org.scalatest.{FunSuite, Matchers}

class CombSortSpec extends FunSuite with Matchers {
   test("Unsorted array") { CombSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array") { CombSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array") { CombSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array") { CombSort.sortCopy(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array") { CombSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
