package org.scalaalgo.combinatorical

import scala.reflect.ClassTag

object Floyd extends CycleFinder {
  override def findCycle[T <: Any : ClassTag](next: T => T, x0: T)(implicit ev: T => Ordered[T]): CycleInformation = {
    var tortoise = next(x0)
    var hare = next(next(x0))

    while (tortoise != hare) {
      tortoise = next(tortoise)
      hare = next(next(hare))
    }

    var positionOfCycle = 0
    tortoise = x0

    while (tortoise != hare) {
      tortoise = next(tortoise)
      hare = next(hare)
      positionOfCycle += 1
    }

    var lengthOfCycle = 1
    hare = next(tortoise)
    while (tortoise != hare) {
      hare = next(hare)
      lengthOfCycle += 1
    }

    CycleInformation(lengthOfCycle, positionOfCycle)
  }
}
