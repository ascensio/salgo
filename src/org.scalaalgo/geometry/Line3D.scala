package org.scalaalgo.geometry

case class Line3D(x0: Vector3D, direction: Vector3D) {
  def getDistance(point: Vector3D) : Double = {
    val crossProductNorm = this.direction.crossProduct(point - this.x0).norm()
    val directionNorm = this.direction.norm()
    crossProductNorm / directionNorm
  }
}

object Line3D {
  def create(firstPoint: Vector3D, secondPoint: Vector3D) : Line3D = {
    Line3D(firstPoint, secondPoint - firstPoint)
  }
}
