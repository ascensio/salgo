package org.salgo.geometry.distance

import org.salgo.geometry.structures.Point2D

object ClosestPairOfPoints {
  def solveByBruteForce(points: Array[Point2D]) : Traversable[(Point2D, Point2D)] = {
    var minimum = Double.PositiveInfinity
    var result = List[(Point2D, Point2D)]()

    for (i <- 0 to points.length - 2) {
      val p = points(i)
      for (j <- i + 1 to points.length - 1) {
        val q = points(j)
        val distance = Point2D.getDistance(p, q)
        if (distance < minimum) {
          minimum = distance
          result = (p, q) :: result
        }
      }
    }

    result
  }


}
