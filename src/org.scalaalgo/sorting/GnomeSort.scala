package org.scalaalgo.sorting

import scala.reflect.ClassTag

object GnomeSort extends GeneralSortingAlgorithm {
  def sort[T <: Any : ClassTag](seq: Array[T])(implicit ev: T => Ordered[T]): Unit = {
    var position = 1
    var last = 0
    val length = seq.length
    while (position < length) {
      if (seq(position) >= seq(position - 1)) {
        if (last != 0) {
          position = last
          last = 0
        }
        position += 1
      }
      else {
        this.swap(seq, position, position - 1)
        if (position > 1) {
          if (last == 0) last = position
          position -= 1
        }
        else {
          position += 1
        }
      }
    }
  }
}
