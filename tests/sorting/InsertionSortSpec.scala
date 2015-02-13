package sorting

import org.scalaalgo.sorting.InsertionSort
import org.scalatest.{FunSuite, Matchers}

class InsertionSortSpec extends FunSuite with Matchers {
   test("Unsorted array") { InsertionSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array") { InsertionSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array") { InsertionSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array") { InsertionSort.sortCopy(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array") { InsertionSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
