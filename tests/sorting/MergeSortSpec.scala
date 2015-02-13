package sorting

import org.scalaalgo.sorting.MergeSort
import org.scalatest.{FunSuite, Matchers}

class MergeSortSpec extends FunSuite with Matchers {
   test("Unsorted array") { MergeSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array") { MergeSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array") { MergeSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array") { MergeSort.sortCopy(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array") { MergeSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
