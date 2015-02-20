package org.salgo.sorting

import scala.reflect.ClassTag

trait GeneralFunctionalSortingAlgorithm extends SortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Seq[T])(implicit ev: T => Ordered[T]) : Seq[T]
}
