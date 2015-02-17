package org.scalaalgo.geometry.structures

case class Point2D(x: Double, y: Double) {
  def + (summand: Point2D) : Point2D = {
    Point2D(this.x + summand.x, this.y + summand.y)
  }

  def - (subtrahend: Point2D) : Point2D = {
    Point2D(this.x - subtrahend.x, this.y - subtrahend.y)
  }

  def * (factor: Point2D) : Point2D = {
    Point2D(this.x * factor.x, this.y * factor.y)
  }

  def norm() : Double = {
    math.sqrt((this.x * this.x) + (this.y * this.y))
  }

  def sum() : Double = {
    this.x + this.y
  }
}

object Point2D {
  def getDistance(p1: Point2D, p2: Point2D) : Double = {
    (p2 - p1).norm()
  }
}
