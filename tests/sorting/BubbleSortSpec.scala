package sorting

import org.scalaalgo.sorting.BubbleSort
import org.scalatest.{FunSuite, Matchers}

class BubbleSortSpec extends FunSuite with Matchers {
   test("Unsorted array") { BubbleSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array") { BubbleSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array") { BubbleSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array") { BubbleSort.sortCopy(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array") { BubbleSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
