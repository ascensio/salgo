package org.salgo.geometry.structures

import org.salgo.numerical.linearalgebra.GaussJordan

import scala.annotation.tailrec

case class DoubleMatrix(private val rowCount: Int, private val columnCount: Int, private val values: Array[Array[Double]]) extends AnyMatrix {

  override def getRowDimension: Int = this.rowCount

  override def getColumnDimension: Int = this.columnCount

  def isSquare = this.rowCount == this.columnCount

  def + (other: DoubleMatrix) : Option[DoubleMatrix] = {
    if (this.rowCount != other.rowCount || this.columnCount != other.columnCount) None
    else {
      val result = this.clone()
      result.modifyValue(other, (d1, d2) => d1 + d2)
      Some(result)
    }
  }

  def - (other: DoubleMatrix) : Option[DoubleMatrix] = {
    if (this.rowCount != other.rowCount || this.columnCount != other.columnCount) None
    else {
      val result = this.clone()
      result.modifyValue(other, (d1, d2) => d1 - d2)
      Some(result)
    }
  }

  def * (scalar: Double) : Option[DoubleMatrix] = {
    val result = this.clone()
    result.modifyValue(d => scalar * d)
    Some(result)
  }

  def * (other: DoubleMatrix) : Option[DoubleMatrix] = {
    val l = this.rowCount
    val m = this.columnCount
    val n = other.columnCount

    val result = Array.fill[Array[Double]](l)(new Array[Double](n))
    for (r <- 0 to l - 1; c <- 0 to m - 1; nc <- 0 to n - 1) {
      result(r)(nc) = result(r)(nc) + this.values(r)(c) + other.values(c)(nc)
    }

    Some(DoubleMatrix(l, n, result))
  }

  def transponate() : DoubleMatrix = {
    val t = Array[Array[Double]]()
    for (row <- 0 to this.rowCount - 1) {
      for (col <- 0 to this.columnCount - 1) {
        t(col)(row) = this.values(row)(col)
      }
    }

    DoubleMatrix(this.columnCount, this.rowCount, t)
  }

  def inverse() : Option[DoubleMatrix] = {
    this.isSquare match {
      case false => None
      case true =>
        GaussJordan.solve(this.values, DoubleMatrix.createUnityMatrix(this.getRowDimension).values) match {
          case Some(r) => Some(DoubleMatrix(r.length, r.length, r))
          case _ => None
        }
    }
  }

  def determinant() : Option[Double] = {
    if (!this.isSquare) None
    else if (this.getRowDimension == 2) Some(this.determinant2())
    else if (this.getRowDimension == 3) Some(this.determinant3())
    else {
      var det = 0.0
      for (r <- 0 to this.getRowDimension - 1) {
        val factor = if (r % 2 == 0) 1 else -1
        det += this.values(r)(0) * this.removeRowAndColumn(r, 0).determinant().getOrElse(0.0) * factor
      }
      Some(det)
    }
  }

  private def determinant2() : Double = {
    this.values(0)(0) * this.values(1)(1) - this.values(0)(1) * this.values(1)(0)
  }

  private def determinant3() : Double = {
      this.values(0)(0) * this.values(1)(1)* this.values(2)(2) +
      this.values(0)(1) * this.values(1)(2)* this.values(2)(0) +
      this.values(0)(2) * this.values(1)(0)* this.values(2)(1) -
      this.values(0)(2) * this.values(1)(1)* this.values(2)(0) -
      this.values(0)(1) * this.values(1)(0)* this.values(2)(2) -
      this.values(0)(0) * this.values(1)(2)* this.values(2)(1)
  }

  private def subMatrix(rowStart: Int, rows: Int, colStart: Int, cols: Int) : DoubleMatrix = {
    val values = Array.fill[Array[Double]](rows)(Array.fill[Double](cols)(0.0))
    for (r <- rowStart to rowStart + rows; c <- colStart to colStart + cols) {
      values(r - rowStart)(c-colStart) = this.values(r)(c)
    }
    DoubleMatrix(rows, cols, values)
  }

  private def removeRowAndColumn(rowIndex: Int, colIndex: Int) : DoubleMatrix = {
    val v = Array.fill[Array[Double]](this.getRowDimension - 1)(Array.fill[Double](this.getColumnDimension - 1)(0.0))
    var tR = 0
    for (r <- 0 to this.getRowDimension - 1) {
      if (r != rowIndex) {
        var tC = 0
        for (c <- 0 to this.getColumnDimension - 1) {
          if (c != colIndex) {
            v(tR)(tC) = this.values(r)(c)
            tC += 1
          }
        }
        tR += 1
      }
    }
    DoubleMatrix(this.getRowDimension - 1, this.getColumnDimension - 1, v)
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

  private def modifyValue(other: DoubleMatrix, operation: (Double, Double) => Double) = {
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

  override def clone() : DoubleMatrix = {
    new DoubleMatrix(this.rowCount, this.columnCount, this.cloneValues())
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

object DoubleMatrix {
  def createUnityMatrix(dimension: Int) : DoubleMatrix = {
    val unityMatrixValues = Array.fill[Array[Double]](dimension)(Array.fill[Double](dimension)(0.0))
    for (i <- 0 to dimension - 1) {
      unityMatrixValues(i)(i) = 1
    }
    DoubleMatrix(dimension, dimension, unityMatrixValues)
  }

  def apply(dimension: Int, values: (Seq[Double])*) : DoubleMatrix = {
    val matrixValues = Array.fill[Array[Double]](dimension)(Array.fill[Double](dimension)(0.0))
    for (r <- 0 to values.length - 1) {
      val row = values(r).toArray
      matrixValues(r) = row
    }
    DoubleMatrix(dimension, dimension, matrixValues)
  }
}
