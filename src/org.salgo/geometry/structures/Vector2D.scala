package org.salgo.geometry.structures

case class Vector2D(x: Double, y: Double) {
  def + (toAdd: Vector2D) : Vector2D = {
    Vector2D(this.x + toAdd.x, this.y + toAdd.y)
  }

  def - (subtrahend: Vector2D) : Vector2D = {
    Vector2D(this.x - subtrahend.x, this.y - subtrahend.y)
  }

  def * (multiplier: Vector2D) : Vector2D = {
    Vector2D(this.x * multiplier.x, this.y * multiplier.y)
  }

  def * (multiplier: Double) : Vector2D = {
    Vector2D(this.x * multiplier, this.y * multiplier)
  }

  def scalarProduct(secondVector: Vector2D) : Double = {
    this.x * secondVector.x + this.y * secondVector.y
  }

  def magnitude() : Double = {
    math.sqrt((this.x * this.x) + (this.y * this.y))
  }

  def normalizedVectors() : (Vector2D, Vector2D) = {
    (Vector2D(-this.y, this.x), Vector2D(this.y, -this.x))
  }
}

object Vector2D {
  def apply(coordinates: (Double, Double)*) : Traversable[Vector2D] = {
    coordinates.foldLeft(Seq[Vector2D]())((seq, c) => seq :+ Vector2D(c._1, c._2))
  }

  def getAngle(first: Vector2D, second: Vector2D) : Double = {
    math.atan2(first.y, first.x) - math.atan2(second.y, second.x)
  }

  def getAngleInDegrees(first: Vector2D, second: Vector2D) : Double = {
    val angle = getAngle(first, second) / math.Pi * 180
    if (angle < 0) angle + 360
    else angle
  }
}
