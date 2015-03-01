package org.salgo.sequences.searching

object BoyerMooreHorspool extends StringSearchAlgorithm {
  override def search(pattern: String, text: String, stopAtFirstMatch: Boolean = false, numberOfCharacters: Int = 256) : Seq[Int] = {
    val shiftBadCharacterMap = this.createBadCharacterShift(pattern, numberOfCharacters)
    val maxPatternIndex = pattern.length - 1
    val maxTextIndex = text.length - 1

    if (maxPatternIndex < 0 || maxTextIndex < 0) Seq.empty[Int]
    else {
      var textLength = text.length
      var patternLength = pattern.length
      var skip = 0
      var result = Seq.empty[Int]

      while (textLength - skip >= patternLength) {
        var i = maxPatternIndex
        var stopLoop = false

        while (!stopLoop && text(skip + i) == pattern(i)) {
          if (i == 0) {
            result = result :+ skip
            if (stopAtFirstMatch) return result
            stopLoop = true
          }
          else {
            i -= 1
          }
        }

        if (!stopLoop) skip += shiftBadCharacterMap(text(skip + i)) - (maxPatternIndex - i)
        else skip += patternLength
      }

      result
    }
  }

  private def createBadCharacterShift(pattern: String, numberOfCharacters: Int) : Array[Int] = {
    val patternLength = pattern.length
    val result = Array.fill[Int](numberOfCharacters)(patternLength)
    for (i <- 0 to patternLength - 2) result(pattern(i)) = patternLength - 1 - i
    result
  }
}
