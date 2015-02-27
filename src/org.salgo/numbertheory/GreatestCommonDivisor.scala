package org.salgo.numbertheory

import scala.annotation.tailrec

object GreatestCommonDivisor {
  @tailrec
  def getByEuclid(a: Int, b: Int) : Int = {
    b match {
      case 0 => a
      case _ => this.getByEuclid(b, a % b)
    }
  }

  def getByBinary(a: Int, b: Int) : Int = {
    (a, b) match {
      case (i, j) if i == j => i
      case (0, j) => j
      case (i, 0) => i
      case (i, j) if i % 2 == 0 =>
        if (j % 2 != 0) this.getByBinary(i >> 1, j)
        else this.getByBinary(i >> 1, j >> 1) << 1
      case (i, j) if j % 2 == 0 => this.getByBinary(i, j >> 1)
      case (i, j) if i > j => this.getByBinary((i - j) >> 1, j)
      case (i, j) =>  this.getByBinary((j - i) >> 1, i)
    }
  }
}
