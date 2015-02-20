package geometry.structures

import org.salgo.geometry.structures.{Line2D, Point2D}
import org.scalatest.{FunSuite, Matchers}

class Line2DSpec  extends FunSuite with Matchers {
  test("Distance of point above") { Line2D(Point2D(1,1), Point2D(2,1)).distance(Point2D(3,3)) shouldEqual 2}
  test("Distance of point above (large)") { Line2D(Point2D(1,1), Point2D(2,1)).distance(Point2D(3,9)) shouldEqual 8}
  test("Distance of point below") { Line2D(Point2D(1,1), Point2D(2,1)).distance(Point2D(3,0)) shouldEqual -1}
  test("Distance of point below (large)") { Line2D(Point2D(1,1), Point2D(2,1)).distance(Point2D(3,-8)) shouldEqual -9}
  test("Distance of point on line") { Line2D(Point2D(1,1), Point2D(2,1)).distance(Point2D(3,1)) shouldEqual 0}
}
