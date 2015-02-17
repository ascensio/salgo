package geometry.distance

import org.scalaalgo.geometry.distance.ClosestPairOfPoints
import org.scalaalgo.geometry.structures.Point2D
import org.scalatest.{FunSuite, Matchers}

class ClosestPairOfPointsSpec extends FunSuite with Matchers {
  test("Brute force with five points") { ClosestPairOfPoints.solveByBruteForce(getPoints) shouldEqual List((Point2D(1.0,1.0),Point2D(3.0,1.0)))}

  private def getPoints : Array[Point2D] = {
    Array[Point2D](Point2D(1, 1), Point2D(3, 1), Point2D(5, 4), Point2D(4, 8), Point2D(6, 2))
  }
}
