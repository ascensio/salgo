package org.scalaalgo.geometry.structures

case class Plane3D(point: Point3D, u: Vector3D, v: Vector3D) {
  def getNormalizedVector: Vector3D = {
    this.u.crossProduct(this.v)
  }

  def getDistance(polarVector: Vector3D) : Double = {
    val normVector = this.getNormalizedVector
    val length = normVector.magnitude()
    val d = this.point.toVector scalarProduct normVector
    ((normVector scalarProduct polarVector) - d) / length
  }
}

object Plane3D {
  def apply(firstPoint: Point3D, secondPoint: Point3D, thirdPoint: Point3D) : Plane3D = {
    Plane3D(firstPoint, secondPoint - firstPoint, thirdPoint - firstPoint)
  }

  def apply(firstPoint: Vector3D, secondPoint: Vector3D, thirdPoint: Vector3D) : Plane3D = {
    Plane3D(Point3D(firstPoint.x, firstPoint.y, firstPoint.z), secondPoint - firstPoint, thirdPoint - firstPoint)
  }
}
