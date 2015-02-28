package org.salgo.common

case class ComplexNumber(r: Double, i: Double) {
  def magnitude() : Double = {
    Math.sqrt(r * r + i * i)
  }

  def + (other: ComplexNumber) : ComplexNumber = {
    ComplexNumber(this.r + other.r, this.i + other.i)
  }

  def - (other: ComplexNumber) : ComplexNumber = {
    ComplexNumber(this.r - other.r, this.i - other.i)
  }

  def * (other: ComplexNumber) : ComplexNumber = {
    ComplexNumber(this.r * other.r - this.i * other.i, this.r * other.i + this.i * other.r)
  }

  def / (other: ComplexNumber) : ComplexNumber = {
    val otherSqr = other.r * other.r + other.i * other.i
    ComplexNumber((this.r * other.r + this.i * other.i) / otherSqr, (this.i * other.r - this.r * other.i) / otherSqr)
  }

  override def toString: String = {
    "real: %f | imaginary: %f".format(this.r, this.i)
  }
}

object ComplexNumber {
}
