package org.salgo.sequences.searching

object KnuthMorrisPratt extends StringSearchAlgorithm {
  override def search(pattern: String, text: String, stopAtFirstMatch: Boolean, numberOfCharacters: Int): Seq[Int] = {
    if (text.length > 0 && pattern.length > 0) {
      var m = 0
      var i = 0
      val lookup = this.buildLookup(pattern)
      var result = Seq.empty[Int]

      while (m + i < text.length) {
        if (pattern(i) == text(m + i)) {
          if (i == pattern.length - 1) {
            if (stopAtFirstMatch) return Seq(m)
            else {
              result = result :+ m
              i = 0
              m += pattern.length
            }
          }
          else i += 1
        }
        else {
          if (lookup(i) > -1) {
            m = m + i - lookup(i)
            i = lookup(i)
          }
          else {
            i = 0
            m += 1
          }
        }
      }

      result
    }
    else Seq.empty[Int]
  }

  private def buildLookup(pattern: String) : Array[Int] = {
    var pos = 2
    var cnd = 0
    var lookup = Array.fill[Int](pattern.length)(0)
    lookup(0) = -1

    while (pos < pattern.length) {
      if (pattern(pos - 1) == pattern(cnd)) {
        cnd += 1
        lookup(pos) = cnd
        pos += 1
      }
      else if (cnd > 0) {
        cnd = lookup(cnd)
      }
      else {
        lookup(pos) = 0
        pos += 1
      }
    }

    lookup
  }
}
