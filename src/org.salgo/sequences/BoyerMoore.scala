package org.salgo.sequences

object BoyerMoore {
  def search(pattern: String, text: String) : Seq[Int] = {
    val shiftBadCharacterMap = this.createBadCharacterShift(pattern)
    val shiftPatternMap = this.createPatternShift(pattern)

    var result = Seq[Int]()
    val pLen = pattern.length
    var textIndex = 0

    while (textIndex + pLen <= text.length) {
      var patternIndex = pLen - 1
      var tempTextIndex = textIndex + patternIndex
      while (patternIndex >= 0 && tempTextIndex >= 0 && pattern(patternIndex) == text(tempTextIndex)) {
        patternIndex -= 1
        tempTextIndex -= 1
      }

      if (patternIndex < 0) {
        result = result :+ textIndex
        textIndex += pLen - 1
      }
      else {
        val shiftBadCharacter = shiftBadCharacterMap(text(textIndex + pLen - 1))
        val shiftPattern = if (patternIndex != pLen - 1) shiftPatternMap(patternIndex) else -1
        var shift = Math.max(shiftBadCharacter, shiftPattern)
        textIndex += shift
      }
    }

    result
  }

  private def createBadCharacterShift(pattern: String) : Array[Int] = {
    val result = Array.fill[Int](Char.MaxValue)(pattern.length)
    for (i <- 0 to pattern.length - 2) result(pattern(i)) = pattern.length - 1 - i
    result
  }

  private def createPatternShift(pattern: String) : Array[Int] = {
    val result = Array.fill[Int](pattern.length)(-1)

    for (pos <- pattern.length - 1 to 0 by -1) {
      result(pos) = this.calculateGoodShift(pattern, pos)
    }

    result
  }

  private def calculateGoodShift(pattern: String, pos: Int) : Int = {
    val valueCount = pattern.length - pos
    var current = pos - 1
    var resultFound = false

    while (current - valueCount > 0 && !resultFound) {

      var stopLoop = false
      var delta = 0
      while (delta < valueCount && !stopLoop) {
        val lookupValue = pattern(pattern.length - 1 - delta)
        val currentValue = pattern(current - delta)
        if (lookupValue != currentValue) {
          stopLoop = true
        }

        delta += 1
      }

      if (stopLoop) {
        current -= 1
      }
      else {
        resultFound = true;
      }
    }

    if (resultFound) current
    else 1
  }
}

object BoyerMoore2 {
  def search(pattern: String, text: String) : Int = {
    if (pattern.length < 1) return -1

    var result = Seq[Int]()

    val charTable = this.buildBadRule(pattern)
    val offsetTable = this.buildGoodRule(pattern)
    val fullShiftTable = this.buildFullShift(pattern)

    var currentIndex = pattern.length - 1
    var previousCurrentIndex = -1

    while (currentIndex < text.length) {
      var patternIndex = pattern.length - 1
      var textIndex = currentIndex

      while (patternIndex >= 0 && textIndex > previousCurrentIndex && pattern(patternIndex) == text(textIndex)) {
        patternIndex -= 1
        textIndex -= 1
      }

      if (patternIndex == -1 || textIndex == previousCurrentIndex) {
        result = result :+ (currentIndex - pattern.length + 1)
        val delta = if (pattern.length > 1) pattern.length - fullShiftTable(1) else 1
        currentIndex += delta
      } else {
        var suffixShift = 0
        if (patternIndex + 1 == pattern.length) {
          suffixShift = 1
        } else if (offsetTable(patternIndex + 1) == -1) {
          suffixShift = pattern.length - fullShiftTable(patternIndex + 1)
        } else {
          suffixShift = pattern.length - offsetTable(patternIndex + 1)
        }
        val charShift = charTable(text(textIndex))
        val shift = Math.max(charShift, suffixShift)
        previousCurrentIndex = if (shift >= patternIndex + 1) currentIndex else previousCurrentIndex
        currentIndex += shift
      }
    }

    result(0)
  }

  private def buildBadRule(pattern: String) : Array[Int] = {
    val patternLength = pattern.length
    val skipArray = Array.fill[Int](Char.MaxValue + 1)(patternLength)

    for(i <- 0 to patternLength - 1) {
      skipArray(pattern(i)) = patternLength - i - 1
    }

    skipArray
  }

  private def buildGoodRule(pattern : String) : Array[Int] = {
    val patternLength = pattern.length
    val prefixPositionArray = Array.fill[Int](patternLength)(-1)
    val n = this.preprocess(pattern.reverse).reverse

    for (j <- 0 to patternLength - 1) {
      val i = patternLength - n(j)
      if (i != patternLength) {
        prefixPositionArray(i) = j
      }
    }

    prefixPositionArray

    /*
    val patternLength = pattern.length
    val prefixPositionArray = Array.fill[Int](patternLength)(-1)

    var lastPrefixPosition = patternLength - 1

    for (pos <- patternLength - 1 to 0 by -1) {
      if (this.isPrefix(pattern, pos + 1)) {
        lastPrefixPosition = pos + 1
      }

      prefixPositionArray(pos) = lastPrefixPosition + patternLength - 1 - pos
    }

    for (i <- 0 to patternLength - 1) {
      val suffixLength = this.suffixLength(pattern, i)
      if (pattern(i - suffixLength) != pattern(patternLength - 1 - suffixLength)) {
        prefixPositionArray(patternLength - 1 - suffixLength) = patternLength - 1 - i + suffixLength
      }
    }

    prefixPositionArray
    */
  }

  private def buildFullShift(pattern: String) : Array[Int] = {
    val patternLength = pattern.length
    val prefixPositionArray = Array.fill[Int](patternLength)(0)
    val z = this.preprocess(pattern)

    var longest = 0
    var index = 0

    for (v <- z.reverse) {
      longest =  Math.max(v, longest)
      prefixPositionArray(index) = longest
      index += 1
    }

    prefixPositionArray
  }

  private def suffixLength(pattern: String, idx: Int) : Int = {
    val len = pattern.length

    /*
    @tailrec
    def matchLengthRecursive(matchCount: Int) : Int = matchCount match {
      case c if (idx - c) >= 0 && (len - 1 - c) >= 0 && pattern(idx - c) == pattern(len - 1 - c) => matchLengthRecursive(c + 1)
      case c => c
    }

    var result = 0
    if (idx == len - 1) result = len
    else result = matchLengthRecursive(0)
*/

    var i = 0
    while (pattern(idx - i) == pattern(len - 1 - i) && (i < idx)) i += 1
    i
  }

  private def isPrefix(pattern: String, pos: Int) : Boolean = {
    val suffixLen = pattern.length - 1 - pos

    var i = pos
    var j = 0
    while (i <= pattern.length - 1) {
      if (pattern(i) != pattern(pos + i)) return false
      i += 1
      j += 1
    }

    true
  }

  private def preprocess(pattern: String) : Array[Int] = {
    if (pattern.length == 0) return Array.empty[Int]
    else if (pattern.length == 1) return Array(1)
    else {
      val result = Array.fill[Int](pattern.length)(0)
      result(0) = pattern.length
      result(1) = this.lengthOfMatch(pattern, 0, 1)
      for (i <- 2 to result(1) + 1) {
        result(i) = result(1) - i + 1
      }

      var l = 0
      var r = 0

      for (i <- 2 + result(1) to pattern.length - 1) {
        if (i <= r) {
          var k = i - 1
          var b = result(k)
          var a = r - i + 1
          if (b < a) {
            result(i) = b
          } else {
            result(i) = b + lengthOfMatch(pattern, a, r + 1)
            l = i
            r = i + result(i) - 1
          }
        } else {
          result(i) = this.lengthOfMatch(pattern, 0, i)
          if (result(i) > 0) {
            l = i
            r = i + result(i) - 1
          }
        }
      }

      result
    }
  }

  private def lengthOfMatch(pattern: String, idxA: Int, idxB: Int) : Int = {
    if (idxA == idxB) return pattern.length - idxA

    var matchCount = 0
    while (idxA + matchCount < pattern.length && idxB + matchCount < pattern.length && pattern(idxA + matchCount) == pattern(idxB + matchCount)) {
      matchCount += 1
    }

    matchCount
  }
}
