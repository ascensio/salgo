package org.scalaalgo.geometry.hulls

import org.scalaalgo.geometry.structures.{Point2D, Vector2D}

object GiftWrapping {
  def solve(points: Traversable[Point2D]) : Traversable[Point2D] = {
    val count = points.count(p => true)
    count match {
      case 0 => Traversable.empty[Point2D]
      case 1 => points
      case n =>
        this.findLeftMostPoint(points) match {
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

  private def findLeftMostPoint(points: Traversable[Point2D]) : Option[Point2D] = {
    points.minBy(p => p.x) match {
      case null => None
      case p => Some(p)
    }
  }
}
