package org.salgo.geometry.areas

import org.salgo.geometry.structures.Point2D

import scala.annotation.tailrec

object Shoelace {
  def calculateArea(points: Traversable[Point2D]) : Double = {
    @tailrec
    def calculateRecursive(pts: Traversable[Point2D], result: Double, first: Point2D) : Double = {
      pts match {
        case Nil => result
        case (p :: Nil) => result
        case (p1 :: p2 :: Nil) =>
          val effectiveFirst = if (first == null) p1 else first
          0.5 * Math.abs(result + (p1.x * p2.y) - (p1.y * p2.x) + p2.x * effectiveFirst.y - p2.y * effectiveFirst.x)
        case (p1 :: p2 :: t) => calculateRecursive(pts.tail, result + (p1.x * p2.y) - (p1.y * p2.x), if (first == null) p1 else first)
      }
    }

    if (points.isEmpty) 0.0
    else calculateRecursive(points, 0.0, null)
  }
}
