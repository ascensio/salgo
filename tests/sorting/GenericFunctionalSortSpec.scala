package sorting

import org.salgo.sorting.GeneralFunctionalSortingAlgorithm
import org.scalatest.{FunSuite, Matchers}

abstract class GenericFunctionalSortSpec extends FunSuite with Matchers {
  test("Sort functional unsorted seq") { this.getSortingAlgorithm.sort(Seq(4, 5, 3, 1, 2)) shouldEqual expectedShort }
  test("Sort functional empty seq") { this.getSortingAlgorithm.sort(Seq[Int]()) shouldEqual Seq[Int]() }
  test("Sort functional already sorted seq") { this.getSortingAlgorithm.sort(Seq(1, 2, 3, 4, 5)) shouldEqual expectedShort }
  test("Sort functional single element seq") { this.getSortingAlgorithm.sort(Seq(5)) shouldEqual Seq(5) }
  test("Sort functional inverse ordered seq") { this.getSortingAlgorithm.sort(Seq(5, 4, 3, 2, 1)) shouldEqual expectedShort }
  test("Sort functional seq with duplicate elements") { this.getSortingAlgorithm.sort(Seq(4, 5, 3, 3, 5)) shouldEqual Seq(3, 3, 4, 5, 5) }
  test("Sort functional seq with single value") { this.getSortingAlgorithm.sort(Seq(4, 4, 4, 4, 4)) shouldEqual Seq(4, 4, 4, 4, 4) }
  test("Sort functional long seq") { this.getSortingAlgorithm.sort(Seq(6, 1, 0, 3, 3, 5, 7, 2, 1, 6, 5, 4 ,3 ,1, 2, 8, 8, 9 ,7)) shouldEqual Seq(0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9) }
  test("Sort functional long list") { this.getSortingAlgorithm.sort(List(6, 1, 0, 3, 3, 5, 7, 2, 1, 6, 5, 4 ,3 ,1, 2, 8, 8, 9 ,7)) shouldEqual List(0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9) }

  def getSortingAlgorithm: GeneralFunctionalSortingAlgorithm

  private def expectedShort = Seq(1, 2, 3, 4, 5)
}
