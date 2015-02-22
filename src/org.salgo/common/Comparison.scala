package org.salgo.common

object Comparison {
  private val defaultEpsilon = 1e-15

  def isApproximatelyEqual(value: Double, comparedValue: Double, epsilon: Double = defaultEpsilon) : Boolean = {
    (value >= comparedValue - epsilon) && (value <= comparedValue + epsilon)
  }
  
  def isApproximatelyEqualOrGreater(value: Double, comparedValue: Double, epsilon: Double = defaultEpsilon) : Boolean = {
    value > comparedValue || isApproximatelyEqual(value, comparedValue, epsilon)
  }

  def areAllApproximatelyEqualOrGreater(values: Traversable[Double], comparedValue: Double, epsilon: Double = defaultEpsilon) : Boolean = {
    values.forall(v => isApproximatelyEqualOrGreater(v, comparedValue, epsilon))
  }

  def isApproximatelyEqualOrSmaller(value: Double, comparedValue: Double, epsilon: Double = defaultEpsilon) : Boolean = {
    value < comparedValue || isApproximatelyEqual(value, comparedValue, epsilon)
  }

  def areAllApproximatelyEqualOrSmaller(values: Traversable[Double], comparedValue: Double, epsilon: Double = defaultEpsilon) : Boolean = {
    values.forall(v => isApproximatelyEqualOrGreater(v, comparedValue, epsilon))
  }
}
