package org.salgo.statistics

import scala.annotation.tailrec

object Covariance {
  def calculateNaive(dataA: Traversable[Double], dataB: Traversable[Double]) : Option[Double] = {
    @tailrec
    def iterate(dataA: Traversable[Double], dataB: Traversable[Double], sumA: Double, sumB: Double, sum: Double, n: Int) : Option[Double] = (dataA, dataB) match {
      case ((ha::ta), Nil) => None
      case (Nil, (hb::tb)) => None
      case (Nil, Nil) => if (n > 0) Some(sum - (sumA * sumB / n) / n) else None
      case ((ha::ta),(hb::tb)) => iterate(ta, tb, sumA + ha, sumB + hb, sum + ha * hb, n + 1)
    }

    iterate(dataA, dataB, 0.0, 0.0, 0.0, 0)
  }

  def calculateShifted(dataA: Traversable[Double], dataB: Traversable[Double]) : Option[Double] = {
    @tailrec
    def iterate(dataA: Traversable[Double], dataB: Traversable[Double], kA: Double, kB: Double, sumA: Double, sumB: Double, sum: Double, n: Int) : Option[Double] = (dataA, dataB) match {
      case ((ha::ta), Nil) => None
      case (Nil, (hb::tb)) => None
      case (Nil, Nil) => if (n > 0) Some(sum - (sumA * sumB / n) / n) else None
      case ((ha::ta),(hb::tb)) => iterate(ta, tb, kA, kB, sumA + ha - kA, sumB + hb - kB, sum + (ha - kA) * (hb - kB), n + 1)
    }

    iterate(dataA.tail, dataB.tail, dataA.head, dataB.head, 0.0, 0.0, 0.0, 0)
  }
}
