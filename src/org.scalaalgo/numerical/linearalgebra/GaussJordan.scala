package org.scalaalgo.numerical.linearalgebra

object GaussJordan {
  def solve(matrix: Array[Array[Double]], d: Array[Double]) : Option[Array[Double]] = {
    val clone = matrix.clone()
    for (r <- 0 to clone.length - 1) {
      clone(r) = clone(r) :+ d(r)
    }

    this.solveCore(clone)(new Array[Double](clone.length))((a, idx, r) => {
      a(idx) = r(r.length - 1)
      a
    })
  }

  def solve(matrix: Array[Array[Double]], d: Array[Array[Double]]) : Option[Array[Array[Double]]] = {
    val clone = matrix.clone()
    for (r <- 0 to clone.length - 1) {
      clone(r) = clone(r) ++ d(r)
    }

    this.solveCore(clone)(new Array[Array[Double]](clone.length))((a, idx, r) => {
      a(idx) = r.slice(r.length - d.length, r.length)
      a
    })
  }

  def solve(matrix: Array[Array[Double]]) : Option[Array[Double]] = {
    this.solveCore(matrix)(new Array[Double](matrix.length))((a, idx, r) => {
      a(idx) = r(r.length - 1)
      a
    })
  }

  private def solveCore[R](matrix: Array[Array[Double]])(default: R)(resultOp: (R, Int, Array[Double]) => R) : Option[R] = {
    this.solveCoreLoop(matrix) match {
      case None => None
      case Some(false) => None
      case Some(true) => Some(matrix.foldLeft((0, default))((a, r) => (a._1 + 1, resultOp(a._2, a._1, r)))._2)
    }
  }

  private def solveCoreLoop(matrix: Array[Array[Double]]) : Option[Boolean] = {
    var columnIndex = 0
    for (currentStep <- 0 to matrix.length - 1) {
      this.findBestRowIndex(matrix, currentStep, columnIndex) match {
        case Some(i) =>
          if (i != currentStep) this.swapRow(matrix, i, currentStep)
          val row = matrix(currentStep)
          val factor = row(columnIndex)
          this.divideRow(row, factor)
          for (otherRowIndex <- 0 to matrix.length - 1) {
            if (currentStep != otherRowIndex) {
              val otherRow = matrix(otherRowIndex)
              val otherFactor = -1 * otherRow(columnIndex)
              val rowToAdd = multiplyRowClone(row, otherFactor)
              this.addToRow(otherRow, rowToAdd)
            }
          }
          columnIndex += 1
        case None => return None
      }
    }

    Some(this.isResolved(matrix))
  }

  private def findBestRowIndex(matrix: Array[Array[Double]], currentStep: Int, columnIndex: Int) : Option[Int] = {
    for (i <- currentStep to matrix.length - 1) {
      if (!this.isZeroOrNan(matrix(i)(columnIndex))) return Some(i)
    }
    None
  }

  private def swapRow(matrix: Array[Array[Double]], first: Int, second: Int) : Unit = {
    val temp = matrix(first)
    matrix(first) = matrix(second)
    matrix(second) = temp
  }

  private def isResolved(matrix: Array[Array[Double]]) : Boolean = {
    for (i <- 0 to matrix.length - 1) {
      val value = matrix(i)(i)
      if (this.isZeroOrNan(value)) false
    }

    true
  }

  private def isZeroOrNan(value: Double) : Boolean = {
    (value < 1e-8 && value > -1e-8) || value == Double.NaN
  }

  private def divideRow(row: Array[Double], factor: Double) : Unit = {
    for (i <- 0 to row.length - 1) row(i) = row(i) / factor
  }

  private def multiplyRowClone(row: Array[Double], factor: Double) : Array[Double] = {
    row.map(d => d * factor)
  }

  private def addToRow(row: Array[Double], rowToAdd: Array[Double]) : Unit = {
    for (i <- 0 to row.length - 1) row(i) = row(i) + rowToAdd(i)
  }
}
