package org.salgo.geometry.hulls

import org.salgo.geometry.structures.{Plane3D, Vector3D}

import scala.collection.mutable.ListBuffer

object QuickHull {
  def calculate(points: Traversable[Vector3D]) : Traversable[Vector3D] = {
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

  def getAndRegisterMinimumAndMaximum(vectors: Traversable[Vector3D], f: Vector3D => Double, result: ListBuffer[Vector3D], remaining: ListBuffer[Vector3D]) : (Vector3D, Vector3D) = {
    val resultMinMax = this.getMinimumAndMaximum(vectors, f)
    val minVector = resultMinMax._1
    val maxVector = resultMinMax._2
    result += minVector
    result += maxVector
    remaining -= minVector
    remaining -= maxVector

    (minVector, maxVector)
  }

  def getMinimumAndMaximum(vectors: Traversable[Vector3D], f: Vector3D => Double) : (Vector3D, Vector3D) = {
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

  def findMaxDistanceEntry(entries: Traversable[(Vector3D, Double)]) : Vector3D = {
    var maxDistanceEntry: (Vector3D, Double) = null
    for (entry <- entries) {
      if (maxDistanceEntry == null || entry._2 > maxDistanceEntry._2) {
        maxDistanceEntry = entry
      }
    }

    maxDistanceEntry._1
  }

  def findPointsWithMaximumDistance(plane: Plane3D, remaining: ListBuffer[Vector3D]): (Vector3D, Vector3D) = {
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

  def removeNotRelevantPoints(plane: Plane3D, newMaxDistance: Vector3D, remaining: ListBuffer[Vector3D]) : Array[Plane3D] = {
    null
  }
}
