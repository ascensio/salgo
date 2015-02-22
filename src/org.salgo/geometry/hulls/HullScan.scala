package org.salgo.geometry.hulls

import org.salgo.common.Default

trait HullScan[P <: AnyRef] {
  protected def findMinAndMaxPoint(points: Traversable[P], f: P => Double)(implicit default: Default[P]) : (P, P) = {
    var resultMin: P = default.value
    var resultMax: P = default.value

    for (v <- points) {
      if (resultMin == null) resultMin = v
      if (resultMax == null) resultMax = v

      f(v) match {
        case x if x < f(resultMin) => resultMin = v
        case x if x > f(resultMax) => resultMax = v
        case _ =>
      }
    }
    (resultMin, resultMax)
  }

  protected def findMinPoint(points: Traversable[P], f: P => Double) : Option[P] = {
    points.minBy(p => f(p)) match {
      case null => None
      case p => Some(p)
    }
  }

  protected def findMaxPoint(points: Traversable[P], f: P => Double) : Option[P] = {
    points.maxBy(p => f(p)) match {
      case null => None
      case p => Some(p)
    }
  }

  def solve(points: Traversable[P]) : Traversable[P]
}
