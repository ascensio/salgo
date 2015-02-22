package org.salgo.geometry.hulls

import org.salgo.geometry.structures.{Point2D, Vector2D}

import scala.annotation.tailrec

object GrahamScan extends HullScan2D {
  def solve(points: Traversable[Point2D]): Traversable[Point2D] = {
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

  def solve2(points: Traversable[Point2D]): Traversable[Point2D] = {
    val distinctPoints = points.toSeq.distinct
    distinctPoints.length match {
      case n if n < 3 => distinctPoints
      case n =>
        val first = distinctPoints.minBy(p => p.y)
        val sortedPoints = distinctPoints.sortBy(p => -1 * Vector2D.getAngle(Vector2D(1,0), p - first))
        val second = sortedPoints(1)
        val hull = Seq(second, first)
        this.solveCore(sortedPoints.tail.tail, hull)
    }
  }

  @tailrec
  def solveCore(points: Seq[Point2D], hull: Seq[Point2D]): Traversable[Point2D] = {
    points match {
      case Nil => hull
      case (h :: t) =>
        this.deleteInvalidHullPoints(h, hull) match {
          case None => Nil
          case Some(newHull) => this.solveCore(t, h +: newHull)
        }
    }
  }

  @tailrec
  private def deleteInvalidHullPoints(current: Point2D, hullPoints: Seq[Point2D]) : Option[Seq[Point2D]] = {
    hullPoints match {
      case Nil => None
      case (h1::Nil) => None
      case (h1::h2::Nil) => if (Point2D.areInClockwiseOrder(h2, h1, current)) None else Some(hullPoints)
      case (h1::h2::tail) => if (Point2D.areInClockwiseOrder(h2, h1, current)) this.deleteInvalidHullPoints(current, h2 :: tail) else Some(hullPoints)
    }
  }
}
