package org.salgo.geometry.structures

case class Plane3D(point: Point3D, u: Vector3D, v: Vector3D) {
  def normalizedVectors: (Vector3D, Vector3D) = {
    val n = this.u.crossProduct(this.v)
    (n, n * -1)
  }

  def distance(polarVector: Vector3D) : Double = {
    val (n1, n2) = this.normalizedVectors
    val length = n1.magnitude()
    val d = this.point.toVector scalarProduct n1
    ((n1 scalarProduct polarVector) - d) / length
  }

  def distance(point: Point3D, normalizedVectorSelection: ((Vector3D, Vector3D)) => Vector3D) : Double = {
    val ns = this.normalizedVectors
    val n = normalizedVectorSelection(ns)
    val length = n.magnitude()
    val d = this.point.toVector scalarProduct n
    ((n scalarProduct point.toVector) - d) / length
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
