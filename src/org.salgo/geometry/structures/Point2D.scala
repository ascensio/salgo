package org.salgo.geometry.structures

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

  def isInTriangle(a: Point2D, b: Point2D, c: Point2D, epsilon: Double) : Boolean = {
    val epsilonSquare = epsilon * epsilon
    if (!this.isInTriangleBoundingBox(a, b, c, epsilon)) false
    else if (this.isInTriangleByDotProduct(a, b, c)) true
    else if (this.distanceSquarePointToSegment(a, b) <= epsilonSquare) true
    else if (this.distanceSquarePointToSegment(b, c) <= epsilonSquare) true
    else if (this.distanceSquarePointToSegment(c, a) <= epsilonSquare) true
    else false
  }

  def isInTriangleBoundingBox(a: Point2D, b: Point2D, c: Point2D, epsilon: Double) : Boolean = {
    if (this.x < math.min(a.x, math.min(b.x, c.x)) - epsilon) false
    else if (this.x > math.max(a.x, math.max(b.x, c.x)) + epsilon) false
    else if (this.y < math.min(a.y, math.min(b.y, c.y)) - epsilon) false
    else if (this.y > math.max(a.y, math.max(b.y, c.y)) + epsilon) false
    else true
  }

  def isInTriangleByDotProduct(a: Point2D, b: Point2D, c: Point2D) : Boolean = {
    val ab = this.side(a, b)
    val bc = this.side(b, c)
    val ca = this.side(c, a)
    (ab > 0 && bc > 0 && ca > 0) || (ab < 0 && bc < 0 && ca < 0)
  }

  private def side(a: Point2D, b: Point2D) : Double = {
    (b.y - a.y) * (this.x - a.x) + (a.x - b.x) * (this.y - a.y)
  }

  private def distanceSquarePointToSegment(a: Point2D, b: Point2D) : Double = {
    val va = a.toVector
    val vb = b.toVector
    val vThis = this.toVector
    val vab = vb - va
    val vthisa = vThis - va
    val lengthSquareAB = vab scalarProduct vab
    val dotProduct = (vthisa scalarProduct vab) / lengthSquareAB
    if (dotProduct < 0) {
      vthisa scalarProduct  vthisa
    }
    else if (dotProduct <= 1) {
      val vathis = va - vThis
      (vathis scalarProduct  vathis) - (dotProduct * dotProduct) * lengthSquareAB
    }
    else {
      val vthisb = vThis - vb
      vthisb scalarProduct  vthisb
    }
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
