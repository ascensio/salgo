package org.scalaalgo.geometry.structures

case class Vector3D(x: Double, y: Double, z: Double) {
  def + (subtrahend: Vector3D) : Vector3D = {
    Vector3D(this.x + subtrahend.x, this.y + subtrahend.y, this.z + subtrahend.z)
  }

  def - (subtrahend: Vector3D) : Vector3D = {
    Vector3D(this.x - subtrahend.x, this.y - subtrahend.y, this.z - subtrahend.z)
  }

  def * (multiplier: Vector3D) : Vector3D = {
    Vector3D(this.x * multiplier.x, this.y * multiplier.y, this.z * multiplier.z)
  }

  def * (multiplier: Double) : Vector3D = {
    Vector3D(this.x * multiplier, this.y * multiplier, this.z * multiplier)
  }

  def x (multiplier: Vector3D) : Vector3D = {
    this crossProduct multiplier
  }

  def crossProduct(multiplier: Vector3D) : Vector3D = {
    val x = this.y * multiplier.z - this.z * multiplier.y
    val y = this.z * multiplier.x - this.x * multiplier.z
    val z = this.x * multiplier.y - this.y * multiplier.x
    Vector3D(x, y, z)
  }

  def scalarProduct(multiplier: Vector3D) : Double = {
    this.x * multiplier.x + this.y * multiplier.y + this.z * multiplier.z
  }

  def magnitude() : Double = {
    math.sqrt((this.x * this.x) + (this.y * this.y) + (this.z * this.z))
  }

  def determinant(secondVector: Vector3D) : Double = {
    (this.x * secondVector.y) - (this.y * secondVector.x)
  }
}

object Vector3D {
  def apply(coordinates: (Double, Double, Double)*) : Traversable[Vector3D] = {
    coordinates.foldLeft(Seq[Vector3D]())((seq, c) => seq :+ Vector3D(c._1, c._2, c._3))
  }
}







