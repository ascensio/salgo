package org.salgo.common

object MathUtils {
  def min[T](values: T*)(implicit ev: T => Ordered[T]) : Option[T] = {
    values.length match {
      case 0 => None
      case 1 => values.collectFirst({ case i => i })
      case n => Some(values.min)
    }
  }

  def max[T](values: T*)(implicit ev: T => Ordered[T]) : Option[T] = {
    values.length match {
      case 0 => None
      case 1 => values.collectFirst({ case i => i })
      case n => Some(values.max)
    }
  }
}
