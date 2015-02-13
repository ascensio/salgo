package sorting

import org.scalaalgo.sorting._
import org.scalatest._

class SwapSortSpec extends FunSuite with Matchers {
  test("Unsorted array") { SwapSort.sortCopy(Array(4, 5, 3, 1, 2)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Empty array") { SwapSort.sortCopy(Array[Int]()) shouldEqual Array[Int]() }
  test("Sorted array") { SwapSort.sortCopy(Array(1, 2, 3, 4, 5)) shouldEqual Array(1, 2, 3, 4, 5) }
  test("Single element array") { SwapSort.sortCopy(Array(5)) shouldEqual Array(5) }
  test("Inverse ordered array") { SwapSort.sortCopy(Array(5, 4, 3, 2, 1)) shouldEqual Array(1, 2, 3, 4, 5) }
}

















