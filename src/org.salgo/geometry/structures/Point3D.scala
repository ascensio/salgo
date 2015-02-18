package org.salgo.geometry.structures

case class Point3D(x: Double, y: Double, z: Double) {
  def + (toAdd: Point3D) : Vector3D = {
    Vector3D(this.x + toAdd.x, this.y + toAdd.y, this.z + toAdd.z)
  }

  def - (subtrahend: Point3D) : Vector3D = {
    Vector3D(this.x - subtrahend.x, this.y - subtrahend.y, this.z - subtrahend.z)
  }

  def toVector : Vector3D = {
    Vector3D(this.x, this.y, this.z)
  }
}

object Point3D {
  def apply(coordinates: (Double, Double, Double)*) : Traversable[Point3D] = {
    coordinates.foldLeft(Seq[Point3D]())((seq, c) => seq :+ Point3D(c._1, c._2, c._3))
  }

  def getDistance(p1: Point3D, p2: Point3D) : Double = {
    (p2 - p1).magnitude()
  }
}