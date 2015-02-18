package org.salgo.combinatorical

import scala.reflect.ClassTag

trait CycleFinder {
  def findCycle[T <: Any : ClassTag](next: T => T, x0: T)(implicit ev: T => Ordered[T]): CycleInformation
}
