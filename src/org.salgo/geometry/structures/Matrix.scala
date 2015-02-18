package org.salgo.geometry.structures

import org.salgo.numerical.linearalgebra.GaussJordan

import scala.annotation.tailrec

case class Matrix(private val rowCount: Int, private val columnCount: Int, private val values: Array[Array[Double]]) extends AnyMatrix {

  override def getRowDimension: Int = this.rowCount

  override def getColumnDimension: Int = this.columnCount

  def isSquare = this.rowCount == this.columnCount

  def + (other: Matrix) : Option[Matrix] = {
    if (this.rowCount != other.rowCount || this.columnCount != other.columnCount) None
    else {
      val result = this.clone()
      result.modifyValue(other, (d1, d2) => d1 + d2)
      Some(result)
    }
  }

  def - (other: Matrix) : Option[Matrix] = {
    if (this.rowCount != other.rowCount || this.columnCount != other.columnCount) None
    else {
      val result = this.clone()
      result.modifyValue(other, (d1, d2) => d1 - d2)
      Some(result)
    }
  }

  def * (scalar: Double) : Option[Matrix] = {
    val result = this.clone()
    result.modifyValue(d => scalar * d)
    Some(result)
  }

  def * (other: Matrix) : Option[Matrix] = {
    val l = this.rowCount
    val m = this.columnCount
    val n = other.columnCount

    val result = Array.fill[Array[Double]](l)(new Array[Double](n))
    for (r <- 0 to l - 1; c <- 0 to m - 1; nc <- 0 to n - 1) {
      result(r)(nc) = result(r)(nc) + this.values(r)(c) + other.values(c)(nc)
    }

    Some(Matrix(l, n, result))
  }

  def transponate() : Matrix = {
    val t = Array[Array[Double]]()
    for (row <- 0 to this.rowCount - 1) {
      for (col <- 0 to this.columnCount - 1) {
        t(col)(row) = this.values(row)(col)
      }
    }

    Matrix(this.columnCount, this.rowCount, t)
  }

  def inverse() : Option[Matrix] = {
    this.isSquare match {
      case false => None
      case true =>
        GaussJordan.solve(this.values, Matrix.createUnityMatrix(this.getRowDimension).values) match {
          case Some(r) => Some(Matrix(r.length, r.length, r))
          case _ => None
        }
    }
  }

  private def cloneValues() : Array[Array[Double]] = {
    this.foldLeft(new Array[Array[Double]](this.rowCount))((res, row) => res :+ row.clone())
  }

  private def modifyValue(operation: Double => Double) = {
    for (r1 <- this.values) {
      for (i <- 0 to this.columnCount - 1) {
        r1(i) = operation(r1(i))
      }
    }
  }

  private def modifyValue(other: Matrix, operation: (Double, Double) => Double) = {
    for (r1 <- this.values; r2 <- other.values) {
      for (i <- 0 to this.columnCount - 1) {
        r1(i) = operation(r1(i), r2(i))
      }
    }
  }

  private def foldLeft[B](default: B)(operation: (B, Array[Double]) => B) = {
    this.foldl[B](0, default, operation)
  }

  private def foldRight[B](default: B, operation: (B, Array[Double]) => B) = {
    this.foldl[B](this.values.length, default, operation)
  }

  @tailrec
  private def foldl[B](index: Int, default: B, operation: (B, Array[Double]) => B): B = {
    if (index == this.values.length) default
    else foldl(index + 1, operation(default, this.values(index)), operation)
  }

  @tailrec
  private def foldr[B](index: Int, default: B, operation: (Array[Double], B) => B): B = {
    if (index == 0) default
    else foldr(index - 1, operation(this.values(index - 1), default), operation)
  }

  override def clone() : Matrix = {
    new Matrix(this.rowCount, this.columnCount, this.cloneValues())
  }

  override def toString : String = {
    val builder = this.foldLeft(StringBuilder.newBuilder)((b, r) => {
      b.append("[")
      b.append(r.foldLeft(StringBuilder.newBuilder)((is, d) => if (is.isEmpty) is.append(d) else is.append("," + d)))
      b.append("]")
    })

    builder.toString()
  }
}

object Matrix {
  def createUnityMatrix(dimension: Int) : Matrix = {
    val unityMatrixValues = Array.fill[Array[Double]](dimension)(Array.fill[Double](dimension)(0.0))
    for (i <- 0 to dimension - 1) {
      unityMatrixValues(i)(i) = 1
    }
    Matrix(dimension, dimension, unityMatrixValues)
  }

  def apply(dimension: Int, values: (Seq[Double])*) : Matrix = {
    val matrixValues = Array.fill[Array[Double]](dimension)(Array.fill[Double](dimension)(0.0))
    for (r <- 0 to values.length - 1) {
      val row = values(r).toArray
      matrixValues(r) = row
    }
    Matrix(dimension, dimension, matrixValues)
  }
}
