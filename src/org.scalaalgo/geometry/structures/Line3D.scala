package org.scalaalgo.geometry.structures

case class Line3D(x0: Point3D, direction: Vector3D) {
  def getDistance(point: Point3D) : Double = {
    val crossProductNorm = this.direction.crossProduct(point - this.x0).magnitude()
    val directionNorm = this.direction.magnitude()
    crossProductNorm / directionNorm
  }
}

object Line3D {
  def apply(firstPoint: Point3D, secondPoint: Point3D) : Line3D = {
    Line3D(firstPoint, secondPoint - firstPoint)
  }
}
