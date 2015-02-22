package org.salgo.geometry.hulls

import org.salgo.geometry.structures._

import scala.collection.mutable.ListBuffer

object QuickHull2D extends HullScan2D {
  override def solve(points: Traversable[Point2D]) : Traversable[Point2D] = {
    val count = points.count(p => true)
    count match {
      case 0 => Traversable.empty[Point2D]
      case n if n <= 3 => points
      case n =>
        val (minX, maxX) = this.findMinAndMaxPoint(points, p => p.x)
        val result = ListBuffer[Point2D](minX, maxX)
        val buffer = ListBuffer[Point2D]()
        buffer.appendAll(points)
        buffer -= minX
        buffer -= maxX
        this.solveRecursive(minX, maxX, buffer, result, v => if (v._1.y > 0) v._1 else v._2)
        this.solveRecursive(minX, maxX, buffer, result, v => if (v._1.y < 0) v._1 else v._2)
        result
    }
  }

  private def solveRecursive(first: Point2D, second: Point2D, buffer: ListBuffer[Point2D], result: ListBuffer[Point2D], vectorSelector: ((Vector2D, Vector2D)) => Vector2D) : Unit = {
    if (buffer.nonEmpty) {
      val line = Line2D(first, second)
      val mapped = buffer.map(p => (p, line.distance(p, vectorSelector))).filter(p => p._2 > 0)
      if (mapped.nonEmpty) {
        val maxDistantPoint = mapped.maxBy(p => p._2)._1
        if (!result.contains(maxDistantPoint)) {
          buffer -= maxDistantPoint
          result += maxDistantPoint
          this.removeInvalidPoints(first, second, maxDistantPoint, buffer)
          this.solveRecursive(first, maxDistantPoint, buffer, result, vectorSelector)
          this.solveRecursive(second, maxDistantPoint, buffer, result, vectorSelector)
        }
      }
    }
  }

  private def removeInvalidPoints(first: Point2D, second: Point2D, third: Point2D, buffer: ListBuffer[Point2D]) : Unit = {
    buffer.filter(p => p.isInTriangle(first, second, third, 1e-10)).foreach(p => buffer -= p)
  }
}


