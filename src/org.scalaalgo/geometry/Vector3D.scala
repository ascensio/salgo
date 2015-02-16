package org.scalaalgo.geometry

case class Vector3D(x: Double, y: Double, z: Double) {
  def crossProduct(multiplier: Vector3D) : Vector3D = {
    val x = this.y * multiplier.z - this.z * multiplier.y
    val y = this.z * multiplier.x - this.x * multiplier.z
    val z = this.x * multiplier.y - this.y * multiplier.x
    Vector3D(x, y, z)
  }

  def + (subtrahend: Vector3D) : Vector3D = {
    Vector3D(this.x + subtrahend.x, this.y + subtrahend.y, this.z + subtrahend.z)
  }

  def - (subtrahend: Vector3D) : Vector3D = {
    Vector3D(this.x - subtrahend.x, this.y - subtrahend.y, this.z - subtrahend.z)
  }

  def * (subtrahend: Vector3D) : Vector3D = {
    Vector3D(this.x * subtrahend.x, this.y * subtrahend.y, this.z * subtrahend.z)
  }

  def norm() : Double = {
    math.sqrt((this.x * this.x) + (this.y * this.y) + (this.z * this.z))
  }

  def sum() : Double = {
    this.x + this.y + this.z
  }
}







