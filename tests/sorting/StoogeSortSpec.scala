package sorting

import org.scalaalgo.sorting.StoogeSort
import org.scalatest.{FunSuite, Matchers}

class StoogeSortSpec extends FunSuite with Matchers {
   test("Unsorted array") { StoogeSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array") { StoogeSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array") { StoogeSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array") { StoogeSort.sortCopy(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array") { StoogeSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
