package org.salgo.geometry.hulls

import org.salgo.geometry.structures.{Point2D, Vector2D}

object GiftWrapping extends HullScan2D {
  def solve(points: Traversable[Point2D]) : Traversable[Point2D] = {
    val count = points.count(p => true)
    count match {
      case 0 => Traversable.empty[Point2D]
      case 1 => points
      case n =>
        this.findMinPoint(points, p => p.x) match {
          case None => Traversable.empty[Point2D]
          case Some(leftMostPoint) =>
            var result = Set[Point2D]()
            var lastPoint = leftMostPoint
            var lastVector = Vector2D(0, -1)

            do {
              var minAngle = Double.PositiveInfinity
              var nextHullPoint: Point2D = null
              var nextVector: Vector2D = null
              for (otherPoint <- points) {
                if (otherPoint != lastPoint) {
                  val vector = otherPoint - lastPoint
                  val angle = Vector2D.getAngleInDegrees(lastVector, vector)
                  if (angle < minAngle) {
                    minAngle = angle
                    nextHullPoint = otherPoint
                    nextVector = vector
                  }
                }
              }
              lastPoint = nextHullPoint
              lastVector = nextVector
              result += nextHullPoint
            } while (lastPoint != leftMostPoint)

            result
        }
    }
  }
}
