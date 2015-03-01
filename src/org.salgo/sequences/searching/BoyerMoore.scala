package org.salgo.sequences.searching

import scala.annotation.tailrec

object BoyerMoore extends StringSearchAlgorithm {
  override def search(pattern: String, text: String, stopAtFirstMatch: Boolean = false, numberOfCharacters: Int = 256) : Seq[Int] = {
    val shiftBadCharacterMap = this.createBadCharacterShift(pattern, numberOfCharacters)
    val shiftPatternMap = this.createPatternShift(pattern)
    val maxPatternIndex = pattern.length - 1
    val maxTextIndex = text.length - 1

    @tailrec
    def searchRecursive(textIndex: Int, indexCorrector: Int, acc: Seq[Int]): Seq[Int] = {
      @tailrec
      def findLastMatchingPatternIndex(pIdx: Int, tIdx: Int): Int = (pIdx - 1, tIdx - 1) match {
        case (p, t) if p < 0 || t < 0 => p
        case (p, t) if pattern(p) == text(t) => findLastMatchingPatternIndex(p, t)
        case (p, t) => p
      }

      textIndex match {
        case ti if ti + maxPatternIndex > maxTextIndex => acc
        case ti =>
          val patternIndex = findLastMatchingPatternIndex(maxPatternIndex - indexCorrector, ti + maxPatternIndex - indexCorrector)
          if (patternIndex < 0) {
            if (stopAtFirstMatch) Seq(ti)
            else searchRecursive(ti + maxPatternIndex, 0, acc :+ ti)
          }
          else {
            val shift = Math.max(shiftBadCharacterMap(text(ti + maxPatternIndex)), shiftPatternMap(patternIndex))
            searchRecursive(ti + shift, if (shift < maxPatternIndex) patternIndex else 0, acc)
          }
      }
    }

    if (maxTextIndex < 0 || maxPatternIndex < 0) Seq.empty[Int]
    else searchRecursive(0, 0, Seq[Int]())
  }

  private def createBadCharacterShift(pattern: String, numberOfCharacters: Int) : Array[Int] = {
    val patternLength = pattern.length
    val result = Array.fill[Int](numberOfCharacters)(patternLength)
    for (i <- 0 to patternLength - 2) result(pattern(i)) = patternLength - 1 - i
    result
  }

  private def createPatternShift(pattern: String) : Array[Int] = {
    val patternLength = pattern.length

    @tailrec
    def findPatternIndex(currentIndex: Int, valueCount: Int) : Int = {

      @tailrec
      def isPatternExistingInCurrentRange(delta: Int) : Boolean = {
        if (delta >= valueCount) true
        else if (pattern(patternLength - 1 - delta) ==  pattern(currentIndex - delta)) isPatternExistingInCurrentRange(delta + 1)
        else false
      }

      if (currentIndex - valueCount <= 0) 1
      else if (isPatternExistingInCurrentRange(0)) currentIndex
      else findPatternIndex(currentIndex - 1, valueCount)
    }

    val result = Array.fill[Int](patternLength)(-1)
    for (pos <- patternLength - 1 to 0 by -1) result(pos) = findPatternIndex(pos - 1, patternLength - pos)
    result
  }
}