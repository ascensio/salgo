package org.salgo.sequences.shuffling

import scala.reflect.ClassTag
import scala.util.Random

object FisherYates {
  def shuffle[T <: Any : ClassTag](seq: Seq[T]) : Seq[T] = {
    val result = seq.toArray
    val random = new Random()
    for (i <- 0 to seq.length - 1) {
      val rdm = random.nextInt(i + 1)
      val tmp = result(i)
      result(i) = result(rdm)
      result(rdm) = tmp
    }
    result
  }
}
