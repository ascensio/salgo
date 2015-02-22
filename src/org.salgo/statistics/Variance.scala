package org.salgo.statistics

import scala.annotation.tailrec

object Variance {
  def calculateNaive(values: Traversable[Double]) : Double = {
    val (n, sumX, sumSqrX) = values./:((0, 0d, 0d))((result, x) => (result._1 + 1, result._2 + x, result._3 + (x * x)))
    if (n > 1) (sumSqrX - (sumX * sumX) / n) / (n - 1) else 0.0
  }

  def calculateShifted(values: Traversable[Double]) : Double = {
    @tailrec
    def calculateCore(data: Traversable[Double], n: Int, sumX: Double, sumSqrX: Double, k: Double) : Double = data match {
      case Nil => (sumSqrX - (sumX * sumX) / n) / (n - 1)
      case (x::t) =>
        val dif = x - k
        calculateCore(t, n + 1, dif + sumX, (dif * dif) + sumSqrX, k)
    }

    if (values.isEmpty) 0.0 else calculateCore(values, 0, 0.0, 0.0, values.head)
  }

  def calculateTwoPass(values: Traversable[Double]) : Double = {
    val (n, sumX) = values./:((0, 0.0))((result, x) => (result._1 + 1, result._2 + x))
    if (n > 1) {
      val mean = sumX / n
      val sumX2 = values./:(0.0)((result, x) => result + (x - mean) * (x - mean))
      sumX2 / (n - 1)
    }
    else 0.0
  }

  def calculateOnline(values: Traversable[Double]) : Double = {
    val (n, mean, m2) = values./:((0, 0.0, 0.0))((result, x) => {
      val n = result._1 + 1
      val delta = x - result._2
      val mean = result._2 + delta / n
      val m2 = result._3 + delta * (x - mean)
      (n, mean, m2)
    })

    if (n > 1) m2 / (n - 1) else 0.0
  }

  def calculateWeighted(weightedValues: Traversable[(Double, Double)]) : Double = {
    val (n, sumWeight, mean, m2) = weightedValues./:((0, 0.0, 0.0, 0.0))((result, x) => {
      val weight = x._2
      val sumWeight = result._2
      val mean = result._3
      val temp = sumWeight + weight
      val delta = x._1 - mean
      val r = delta * weight / temp
      val m2 = result._4 + sumWeight * delta * r
      (result._1 + 1, temp, mean + r, m2)
    })

    if (n > 1) m2 / sumWeight * (n / (n - 1)) else 0.0
  }
}
