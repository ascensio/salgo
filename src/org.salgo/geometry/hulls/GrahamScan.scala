package org.salgo.geometry.hulls

import org.salgo.geometry.structures.{Point2D, Vector2D}

object GrahamScan extends HullScan2D {
  override def solve(points: Traversable[Point2D]): Traversable[Point2D] = {
    if (points.isEmpty){
      points
    } else {
      val pointArray = points.toArray
      val first = pointArray.minBy(p => p.y)
      val sortedArray = pointArray.distinct.sortBy(p => -1 * Vector2D.getAngle(Vector2D(1,0), p - first))
      sortedArray.length match {
        case n if n <= 3 => points
        case n =>
          val second = sortedArray(1)
          var hull = List(second, first)

          for (idx <- 2 to n - 1) {
            var previousHullPoint = hull.head
            var prePreviousHullPoint = hull.tail.head
            val currentPoint = sortedArray(idx)

            while (Point2D.areInClockwiseOrder(prePreviousHullPoint, previousHullPoint, currentPoint)) {
              hull = hull.tail
              previousHullPoint = hull.head
              prePreviousHullPoint = hull.tail.head
            }

            hull = currentPoint :: hull
          }

          hull
      }
    }
  }
}
