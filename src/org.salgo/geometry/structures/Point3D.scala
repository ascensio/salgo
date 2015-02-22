package org.salgo.geometry.structures

import org.salgo.common.{Comparison, MathUtils}

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

  def isInTetrahedra(a: Point3D, b: Point3D, c: Point3D, d: Point3D, epsilon: Double) : Boolean = {
    //if (!this.isInTetrahedraBoundingBox(a, b, c, d, epsilon)) false
    //else
    if (this.isInTetrahedraByDotProduct(a, b, c, d, epsilon)) true
    else false
  }

  def isInTetrahedraBoundingBox(a: Point3D, b: Point3D, c: Point3D, d: Point3D, epsilon: Double) : Boolean = {
    if (this.x < MathUtils.min(a.x, b.x, c.x, d.x).getOrElse(0.0) - epsilon) false
    else if (this.x > MathUtils.max(a.x, b.x, c.x, d.x).getOrElse(0.0) + epsilon) false
    else if (this.y < MathUtils.min(a.y, b.y, c.y, d.y).getOrElse(0.0) - epsilon) false
    else if (this.y > MathUtils.max(a.y, b.y, c.y, d.y).getOrElse(0.0) + epsilon) false
    else if (this.z < MathUtils.min(a.z, b.z, c.z, d.z).getOrElse(0.0) - epsilon) false
    else if (this.z > MathUtils.max(a.z, b.z, c.z, d.z).getOrElse(0.0) + epsilon) false
    else true
  }

  def isInTetrahedraByDotProduct(a: Point3D, b: Point3D, c: Point3D, d: Point3D, epsilon: Double) : Boolean = {
     val d0 = this.getDeterminant(a, b, c, d)
     val d1 = this.getDeterminant(this, b, c, d)
     val d2 = this.getDeterminant(a, this, c, d)
     val d3 = this.getDeterminant(a, b, this, d)
     val d4 = this.getDeterminant(a, b, c, this)

    if (Comparison.isApproximatelyEqualOrGreater(d0, 0d)) {
      if (!Comparison.isApproximatelyEqualOrGreater(d1, 0d)) false
      else if (!Comparison.isApproximatelyEqualOrGreater(d2, 0d)) false
      else if (!Comparison.isApproximatelyEqualOrGreater(d3, 0d)) false
      else if (!Comparison.isApproximatelyEqualOrGreater(d4, 0d)) false
      else true
    } else {
      if (!Comparison.isApproximatelyEqualOrSmaller(d1, 0d)) false
      else if (!Comparison.isApproximatelyEqualOrSmaller(d2, 0d)) false
      else if (!Comparison.isApproximatelyEqualOrSmaller(d3, 0d)) false
      else if (!Comparison.isApproximatelyEqualOrSmaller(d4, 0d)) false
      else true
    }
  }

  private def getDeterminant(a: Point3D, b: Point3D, c: Point3D, d: Point3D, default: Double = 0.0) : Double = {
    DoubleMatrix(4, Seq(a.x, a.y, a.z, 1), Seq(b.x, b.y, b.z, 1), Seq(c.x, c.y, c.z, 1), Seq(d.x, d.y, d.z, 1)).determinant().getOrElse(default)
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