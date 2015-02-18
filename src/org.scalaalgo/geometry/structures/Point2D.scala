package org.scalaalgo.geometry.structures

case class Point2D(x: Double, y: Double) {
  def + (toAdd: Point2D) : Vector2D = {
    Vector2D(this.x + toAdd.x, this.y + toAdd.y)
  }

  def - (subtrahend: Point2D) : Vector2D = {
    Vector2D(this.x - subtrahend.x, this.y - subtrahend.y)
  }

  def toVector : Vector2D = {
    Vector2D(this.x, this.y)
  }
}

object Point2D {
  def apply(coordinates: (Double, Double)*) : Traversable[Point2D] = {
    coordinates.foldLeft(Seq[Point2D]())((seq, c) => seq :+ Point2D(c._1, c._2))
  }

  def getDistance(p1: Point2D, p2: Point2D) : Double = {
    (p2 - p1).magnitude()
  }
}
