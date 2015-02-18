package org.salgo.sequences.matching

object Bitap {
  def search(text: String, pattern: String) : Option[Int] = {
    val patternLength = pattern.length
    if (patternLength > 0) {
      val bits = Array.fill[Boolean](patternLength + 1)(false)
      bits(0) = true

      for (i <- 0 to text.length - 1 by 1) {
        for (k <- patternLength to 1 by -1) {
          bits(k) = bits(k - 1) & (text(i) == pattern(k - 1))
        }
        if (bits(patternLength)) return Some(i)
      }
    }

    None
  }


}
