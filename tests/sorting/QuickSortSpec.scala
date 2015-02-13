package sorting

import org.scalaalgo.sorting.QuickSort
import org.scalatest.{FunSuite, Matchers}

class QuickSortSpec extends FunSuite with Matchers {
   test("Unsorted array") { QuickSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array") { QuickSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array") { QuickSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array") { QuickSort.sortCopy(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array") { QuickSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }

   test("Unsorted array (tailrec)") { QuickSort.sortCopyTailRec(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array (tailrec)") { QuickSort.sortCopyTailRec(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array (tailrec)") { QuickSort.sortCopyTailRec(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array (tailrec)") { QuickSort.sortCopyTailRec(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array (tailrec)") { QuickSort.sortCopyTailRec(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
