package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, QuickSort}

class QuickSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = QuickSort

   test("Unsorted array (tailrec)") { QuickSort.sortCopyTailRec(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Empty array (tailrec)") { QuickSort.sortCopyTailRec(Array[Int]()) shouldEqual Array[Int]() }
   test("Sorted array (tailrec)") { QuickSort.sortCopyTailRec(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
   test("Single element array (tailrec)") { QuickSort.sortCopyTailRec(Array(5)) shouldEqual Array(5) }
   test("Inverse ordered array (tailrec)") { QuickSort.sortCopyTailRec(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
 }
