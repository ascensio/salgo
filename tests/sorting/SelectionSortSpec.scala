package sorting

import org.scalaalgo.sorting.SelectionSort
import org.scalatest.{Matchers, FunSuite}

class SelectionSortSpec extends FunSuite with Matchers {
   test("Unsorted array") { SelectionSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array") { SelectionSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array") { SelectionSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array") { SelectionSort.sortCopy(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array") { SelectionSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
