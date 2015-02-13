import org.scalaalgo.sorting._
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class InsertionSortSpec extends FunSuite with Matchers {
  test("Unsorted array") { InsertionSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Empty array") { InsertionSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sorted array") { InsertionSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Single element array") { InsertionSort.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Inverse ordered array") { InsertionSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
}

class SelectionSortSpec extends FunSuite with Matchers {
  test("Unsorted array") { SelectionSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Empty array") { SelectionSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sorted array") { SelectionSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Single element array") { SelectionSort.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Inverse ordered array") { SelectionSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
}

class SwapSortSpec extends FunSuite with Matchers {
  test("Unsorted array") { SwapSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Empty array") { SwapSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sorted array") { SwapSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Single element array") { SwapSort.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Inverse ordered array") { SwapSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
}

class StoogeSortSpec extends FunSuite with Matchers {
  test("Unsorted array") { StoogeSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Empty array") { StoogeSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sorted array") { StoogeSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Single element array") { StoogeSort.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Inverse ordered array") { StoogeSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
}

class SlowSortSpec extends FunSuite with Matchers {
  test("Unsorted array") { SlowSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Empty array") { SlowSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sorted array") { SlowSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Single element array") { SlowSort.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Inverse ordered array") { SlowSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
}


class ShakerSortSpec extends FunSuite with Matchers {
  test("Unsorted array") { ShakerSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Empty array") { ShakerSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sorted array") { ShakerSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Single element array") { ShakerSort.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Inverse ordered array") { ShakerSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
}

class MergeSortSpec extends FunSuite with Matchers {
  test("Unsorted array") { MergeSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Empty array") { MergeSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sorted array") { MergeSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Single element array") { MergeSort.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Inverse ordered array") { MergeSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
}

class CombSortSpec extends FunSuite with Matchers {
  test("Unsorted array") { CombSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Empty array") { CombSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sorted array") { CombSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Single element array") { CombSort.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Inverse ordered array") { CombSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
}

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

