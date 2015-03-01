package geometry.areas

import org.salgo.geometry.areas.Shoelace
import org.salgo.geometry.structures.Point2D
import org.scalatest.{FunSuite, Matchers}

class ShoelaceSpec extends FunSuite with Matchers {
  test("Empty points") { Shoelace.calculateArea(Seq.empty[Point2D]) shouldEqual 0.0}
  test("Single point") { Shoelace.calculateArea(Seq(Point2D(1,1))) shouldEqual 0.0}
  test("Line") { Shoelace.calculateArea(Seq(Point2D(1,1), Point2D(2,2))) shouldEqual 0.0}
  test("Triangle") { Shoelace.calculateArea(Seq(Point2D(2,1), Point2D(4,5), Point2D(7, 8))) shouldEqual 3.0}
  test("Complex polygon") { Shoelace.calculateArea(getPoints) shouldEqual 30.0}

  private def getPoints : Seq[Point2D] = {
    Seq[Point2D](Point2D(3, 4), Point2D(5, 11), Point2D(12, 8), Point2D(9, 5), Point2D(5, 6))
  }
}
