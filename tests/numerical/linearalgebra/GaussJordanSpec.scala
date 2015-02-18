package numerical.linearalgebra

import org.salgo.numerical.linearalgebra.GaussJordan
import org.scalatest.{FunSuite, Matchers}

class GaussJordanSpec extends FunSuite with Matchers {
  test("Solvable simple system") { GaussJordan.solve(this.createSolvableSimpleMatrix).get shouldEqual Array[Double](0.5, -0.5, 0) }
  test("Solvable simple system 2") { GaussJordan.solve(this.createSolvableSimpleMatrix2).get shouldEqual Array[Double](2, 0, -1) }
  //test("Solvable complex system") { GaussJordan.solve(this.createSolvableMatrix).get shouldEqual Array[Double](-0.5965, 0.4525, 4.386, 0.7385, 0.582) }
  test("Not solvable simple system") { GaussJordan.solve(this.createNotSolvableSimpleMatrix) shouldEqual None }
  test("Not solvable simple system 2") { GaussJordan.solve(this.createNotSolvableSimpleMatrix2) shouldEqual None }
  test("Not solvable with multi solutions") { GaussJordan.solve(this.createMultiSolvableSimpleMatrix) shouldEqual None }

  private def createSolvableSimpleMatrix : Array[Array[Double]] = {
    Array[Array[Double]](
      Array[Double](1, 1, 1, 0),
      Array[Double](4, 2, 1, 1),
      Array[Double](9, 3, 1, 3)
    )
  }

  private def createSolvableSimpleMatrix2 : Array[Array[Double]] = {
    Array[Array[Double]](
      Array[Double](2, -2,  1, 3),
      Array[Double](3,  1, -1, 7),
      Array[Double](1, -3,  2, 0)
    )
  }

  private def createSolvableMatrix : Array[Array[Double]] = {
    Array[Array[Double]](
      Array[Double](3, 3, 0, 6, 0, 4),
      Array[Double](4, 0, 1, 0, 0, 2),
      Array[Double](5, 3, 1, 0, 9, 8),
      Array[Double](4, 7, 0, 3, 0, 3),
      Array[Double](8, 1, 0, 0, 9, 1)
    )
  }
  private def createNotSolvableSimpleMatrix : Array[Array[Double]] = {
    Array[Array[Double]](
      Array[Double](0, 1, 1, 0),
      Array[Double](0, 2, 1, 1),
      Array[Double](0, 3, 1, 3)
    )
  }

  private def createNotSolvableSimpleMatrix2 : Array[Array[Double]] = {
    Array[Array[Double]](
      Array[Double]( 2, -4,  1, -4),
      Array[Double]( 4, -8,  7,  2),
      Array[Double](-2,  4, -3,  5)
    )
  }

  private def createMultiSolvableSimpleMatrix : Array[Array[Double]] = {
    Array[Array[Double]](
      Array[Double]( 3,  6, -9, 15),
      Array[Double]( 2,  4, -6, 10),
      Array[Double](-2, -3,  4, -6)
    )
  }
}

