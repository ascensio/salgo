package org.scalaalgo.geometry

case class Plane3D(point: Vector3D, u: Vector3D, v: Vector3D) {
  def getNormalizedVector: Vector3D = {
    this.u.crossProduct(this.v)
  }

  def getCoordinateTemplate : (Vector3D, Double) = {
    val n = this.getNormalizedVector
    val multipliedPoint = this.point * n
    (n, multipliedPoint.sum())
  }

  def getDistance(p: Vector3D) : Double = {
    val normVector = this.getNormalizedVector
    val length = normVector.norm()

    val factors = this.point * normVector
    val d = factors.sum()

    ((normVector * p).sum() - d) / length
  }
}

object Plane3D {
  def create(firstPoint: Vector3D, secondPoint: Vector3D, thirdPoint: Vector3D) : Plane3D = {
    Plane3D(firstPoint, secondPoint - firstPoint, thirdPoint - firstPoint)
  }
}
