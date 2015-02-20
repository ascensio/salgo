package org.salgo.geometry.structures

case class Line2D (start: Point2D, direction: Vector2D) {
  def distance(point: Point2D) : Double = {
    this.distance(point, v => v._1)
  }

  def distance(point: Point2D, normalizedVectorSelection: ((Vector2D, Vector2D)) => Vector2D) : Double = {
    val ns = this.direction.normalizedVectors()
    val n = normalizedVectorSelection(ns)
    val n0 = start.toVector * n
    (n.x * point.x + n.y * point.y - n0.x - n0.y) / n.magnitude()
  }

  def distanceAbs(point: Point2D) : Double = {
   math.abs(this.distance(point))
  }
}

object Line2D  {
  def apply(firstPoint: Point2D, secondPoint: Point2D) : Line2D = {
    Line2D(firstPoint, secondPoint - firstPoint)
  }
}
