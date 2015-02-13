package sorting

import org.scalaalgo.sorting.ShakerSort
import org.scalatest.{Matchers, FunSuite}

class ShakerSortSpec extends FunSuite with Matchers {
   test("Unsorted array") { ShakerSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array") { ShakerSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array") { ShakerSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array") { ShakerSort.sortCopy(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array") { ShakerSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
