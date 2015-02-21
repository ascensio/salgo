package org.salgo.geometry.hulls

import org.salgo.geometry.structures._

import scala.collection.mutable.ListBuffer

object QuickHull {
  def solve2D(points: Traversable[Point2D]) : Traversable[Point2D] = {
    val count = points.count(p => true)
    count match {
      case 0 => Traversable.empty[Point2D]
      case n if n <= 3 => points
      case n =>
        val minMax = this.findLeftAndRightStartPoint(points)
        val result = ListBuffer[Point2D](minMax._1, minMax._2)
        val buffer = ListBuffer[Point2D]()
        buffer.appendAll(points)
        buffer -= minMax._1
        buffer -= minMax._2
        this.solveRecursive(minMax._1, minMax._2, buffer, result, v => if (v._1.y > 0) v._1 else v._2)
        this.solveRecursive(minMax._1, minMax._2, buffer, result, v => if (v._1.y < 0) v._1 else v._2)
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

  private def findLeftAndRightStartPoint(points: Traversable[Point2D]) : (Point2D, Point2D) = {
    val min = points.minBy(p => p.x)
    val max = points.maxBy(p => p.x)
    (min, max)
  }

  private def removeInvalidPoints(first: Point2D, second: Point2D, third: Point2D, buffer: ListBuffer[Point2D]) : Unit = {
    buffer.filter(p => p.isInTriangle(first, second, third, 1e-10)).foreach(p => buffer -= p)
  }

  def solve3D(points: Traversable[Point3D]) : Traversable[Point3D] = {
    val count = points.count(p => true)
    count match {
      case 0 => Traversable.empty[Point3D]
      case n if n <= 4 => points
      case n =>
        val (minX, maxX) = this.findMinAndMaxStartPoint(points, p => p.x)
        val (minY, maxY) = this.findMinAndMaxStartPoint(points, p => p.y)
        val result = ListBuffer[Point3D](minX, maxX, minY).distinct
        val buffer = ListBuffer[Point3D]()
        buffer.appendAll(points)
        buffer -= minX
        buffer -= maxX
        buffer -= minY

        this.solveRecursive(minX, maxX, minY, buffer, result, v => if (v._1.y > 0) v._1 else v._2)
        this.solveRecursive(minX, maxX, minY, buffer, result, v => if (v._1.y < 0) v._1 else v._2)
        result
    }
  }

  private def solveRecursive(first: Point3D, second: Point3D, third: Point3D, buffer: ListBuffer[Point3D], result: ListBuffer[Point3D], vectorSelector: ((Vector3D, Vector3D)) => Vector3D) : Unit = {
    if (buffer.nonEmpty) {
      val plane = Plane3D(first, second, third)
      val mapped = buffer.map(p => (p, plane.distance(p, vectorSelector))).filter(p => p._2 > 0)
      if (mapped.nonEmpty) {
        val maxDistantPoint = mapped.maxBy(p => p._2)._1
        if (!result.contains(maxDistantPoint)) {
          buffer -= maxDistantPoint
          result += maxDistantPoint
          this.removeInvalidPoints(first, second, third, maxDistantPoint, buffer)
          this.solveRecursive(first, second, maxDistantPoint, buffer, result, vectorSelector)
          this.solveRecursive(second, third, maxDistantPoint, buffer, result, vectorSelector)
          this.solveRecursive(third, first, maxDistantPoint, buffer, result, vectorSelector)
        }
      }
    }
  }

  private def findMinAndMaxStartPoint(points: Traversable[Point3D], f: Point3D => Double) : (Point3D, Point3D) = {
    var resultMin: Point3D = null
    var resultMax: Point3D = null

    for (v <- points) {
      if (resultMin == null) resultMin = v
      if (resultMax == null) resultMax = v

      f(v) match {
        case x if x < f(resultMin) => resultMin = v
        case x if x > f(resultMax) => resultMax = v
        case _ =>
      }
    }

    (resultMin, resultMax)
  }

  private def removeInvalidPoints(first: Point3D, second: Point3D, third: Point3D, forth: Point3D, buffer: ListBuffer[Point3D]) : Unit = {
    buffer.filter(p => p.isInTetrahedra(first, second, third, forth, 1e-10)).foreach(p => buffer -= p)
  }
}
