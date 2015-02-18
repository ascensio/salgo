package org.salgo.combinatorical

import scala.reflect.ClassTag

object Brent extends CycleFinder {
  override def findCycle[T <: Any : ClassTag](next: T => T, x0: T)(implicit ev: T => Ordered[T]): CycleInformation = {
    var power = 1
    var lengthOfCycle = 1
    var tortoise = x0
    var hare = next(x0)

    while (tortoise != hare) {
      if (power == lengthOfCycle) {
        tortoise = hare
        power *= 2
        lengthOfCycle = 0
      }

      hare = next(hare)
      lengthOfCycle += 1
    }

    var positionOfCycle = 0
    tortoise = x0
    hare = x0

    for (i <- 0 to lengthOfCycle - 1 by 1) {
      hare = next(hare)
    }

    val lh = hare
    val lT = tortoise

    while (tortoise != hare) {
      tortoise = next(tortoise)
      hare = next(hare)
      positionOfCycle += 1
    }

    CycleInformation(lengthOfCycle, positionOfCycle)
  }
}


