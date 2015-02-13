package sorting

import org.scalaalgo.sorting.SlowSort
import org.scalatest.{Matchers, FunSuite}

class SlowSortSpec extends FunSuite with Matchers {
   test("Unsorted array") { SlowSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array") { SlowSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array") { SlowSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array") { SlowSort.sortCopy(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array") { SlowSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
