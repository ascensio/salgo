package geometry.structures

import org.salgo.geometry.structures.Matrix
import org.scalatest.{FunSuite, Matchers}

class MatrixSpec extends FunSuite with Matchers {
  test("Inverse") { getSimpleMatrix.inverse().get shouldBe getExpectedInverseSimpleMatrix}

  private def getSimpleMatrix : Matrix = {
    Matrix(3, Seq(1,2,0),Seq(2,4,1),Seq(2,1,0))
  }

  private def getExpectedInverseSimpleMatrix : Matrix = {
    Matrix(3, Seq(-1d/3d,0d,2d/3d),Seq(2d/3d,0d,-1d/3d),Seq(-2d,1d,0d))
  }
}
