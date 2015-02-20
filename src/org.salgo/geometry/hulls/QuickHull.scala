package org.salgo.geometry.hulls

import org.salgo.geometry.structures._

import scala.collection.mutable.ListBuffer

object QuickHull {

  private def calculate(points: Traversable[Vector3D]) : Traversable[Vector3D] = {
    val result = ListBuffer[Vector3D]()
    val remaining = ListBuffer[Vector3D]()
    remaining ++= points

    val resultMinMaxForX = this.getAndRegisterMinimumAndMaximum(points, p => p.x, result, remaining)
    val xMin = resultMinMaxForX._1
    val xMax = resultMinMaxForX._2

    val resultMinMaxForY = this.getAndRegisterMinimumAndMaximum(points, p => p.y, result, remaining)
    val yMin = resultMinMaxForY._1
    val yMax = resultMinMaxForY._2

    val resultMinMaxForZ = this.getAndRegisterMinimumAndMaximum(points, p => p.z, result, remaining)
    val zMin = resultMinMaxForZ._1
    val zMax = resultMinMaxForZ._2

    val planes = List[Plane3D](
      Plane3D(xMin, yMin, zMin),
      Plane3D(xMin, yMin, zMax),
      Plane3D(xMin, yMax, zMin),
      Plane3D(xMin, yMax, zMax),
      Plane3D(xMax, yMin, zMin),
      Plane3D(xMax, yMin, zMax),
      Plane3D(xMax, yMax, zMin),
      Plane3D(xMax, yMax, zMax)
    )

    val firstPlanePoint = xMin
    val secondPlanePoint = xMin
    //val thirdPlanePoint = Vector3D()
    //val plane = Plane3D.create(firstPlanePoint, secondPlanePoint, thirdPlanePoint)

    //val maxDistancePoints = this.findPointsWithMaximumDistance(plane, remaining)
    //val pointMaxDistanceBelow = maxDistancePoints._1
    //val pointMaxDistanceAbove = maxDistancePoints._2
    result
  }

  private def getAndRegisterMinimumAndMaximum(vectors: Traversable[Vector3D], f: Vector3D => Double, result: ListBuffer[Vector3D], remaining: ListBuffer[Vector3D]) : (Vector3D, Vector3D) = {
    val resultMinMax = this.getMinimumAndMaximum(vectors, f)
    val minVector = resultMinMax._1
    val maxVector = resultMinMax._2
    result += minVector
    result += maxVector
    remaining -= minVector
    remaining -= maxVector

    (minVector, maxVector)
  }

  private def getMinimumAndMaximum(vectors: Traversable[Vector3D], f: Vector3D => Double) : (Vector3D, Vector3D) = {
    var resultMin: Vector3D = null
    var resultMax: Vector3D = null

    for (v <- vectors) {
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

  private def findMaxDistanceEntry(entries: Traversable[(Vector3D, Double)]) : Vector3D = {
    var maxDistanceEntry: (Vector3D, Double) = null
    for (entry <- entries) {
      if (maxDistanceEntry == null || entry._2 > maxDistanceEntry._2) {
        maxDistanceEntry = entry
      }
    }

    maxDistanceEntry._1
  }

  private def findPointsWithMaximumDistance(plane: Plane3D, remaining: ListBuffer[Vector3D]): (Vector3D, Vector3D) = {
    val distanceMapAbove = scala.collection.mutable.Map[Vector3D, Double]()
    val distanceMapBelow = scala.collection.mutable.Map[Vector3D, Double]()
    for (remVector <- remaining) {
      val distance = plane.getDistance(remVector)
      distance match {
        case d if d < 0 => distanceMapBelow += remVector -> math.abs(distance)
        case d => distanceMapAbove += remVector -> distance
      }
    }

    val maxDistanceAbove = this.findMaxDistanceEntry(distanceMapAbove)
    val maxDistanceBelow = this.findMaxDistanceEntry(distanceMapBelow)
    (maxDistanceBelow, maxDistanceAbove)
  }

  private def removeNotRelevantPoints(plane: Plane3D, newMaxDistance: Vector3D, remaining: ListBuffer[Vector3D]) : Array[Plane3D] = {
    null
  }



  def solve(points: Traversable[Point2D]) : Traversable[Point2D] = {
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
}
