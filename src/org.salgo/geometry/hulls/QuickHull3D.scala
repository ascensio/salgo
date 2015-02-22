package org.salgo.geometry.hulls

import org.salgo.geometry.structures.{Plane3D, Point3D, Vector3D}

import scala.collection.mutable.ListBuffer

object QuickHull3D extends HullScan3D {
  override def solve(points: Traversable[Point3D]) : Traversable[Point3D] = {
    val count = points.count(p => true)
    count match {
      case 0 => Traversable.empty[Point3D]
      case n if n <= 4 => points
      case n =>
        val (first, second, third) = this.findStartPoints(points)
        val result = ListBuffer[Point3D](first, second, third).distinct
        val buffer = ListBuffer[Point3D]()
        buffer.appendAll(points)
        buffer -= first
        buffer -= second
        buffer -= third

        this.solveRecursive(first, second, third, buffer, result, v => if (v._1.y > 0) v._1 else v._2)
        this.solveRecursive(first, second, third, buffer, result, v => if (v._1.y < 0) v._1 else v._2)
        result
    }
  }

  private def findStartPoints(points: Traversable[Point3D]) : (Point3D, Point3D, Point3D) = {
    val (minX, maxX) = this.findMinAndMaxPoint(points, p => p.x)
    val (minY, maxY) = this.findMinAndMaxPoint(points, p => p.y)
    val (minZ, maxZ) = this.findMinAndMaxPoint(points, p => p.z)
    val result = Seq(minX, maxX, minY, maxY, minZ, maxZ).distinct.take(3)
    (result(0), result(1), result(2))
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

  private def removeInvalidPoints(first: Point3D, second: Point3D, third: Point3D, forth: Point3D, buffer: ListBuffer[Point3D]) : Unit = {
    buffer.filter(p => p.isInTetrahedra(first, second, third, forth, 1e-10)).foreach(p => buffer -= p)
  }
}
