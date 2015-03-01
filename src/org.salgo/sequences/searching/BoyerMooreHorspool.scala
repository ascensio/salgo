package org.salgo.sequences.searching

import scala.annotation.tailrec

object BoyerMooreHorspool extends StringSearchAlgorithm {
  override def search(pattern: String, text: String, stopAtFirstMatch: Boolean = false, numberOfCharacters: Int = 256) : Seq[Int] = {
    lazy val shiftBadCharacterMap = this.createBadCharacterShift(pattern, numberOfCharacters)
    val patternLength = pattern.length
    val maxPatternIndex = patternLength - 1
    val textLength = text.length
    val maxTextIndex = textLength - 1

    @tailrec
    def searchRecursive(offset: Int, acc: Seq[Int]): Seq[Int] = {
      @tailrec
      def findLastMatchingPatternIndex(pIdx: Int, tIdx: Int): Int = (pIdx - 1, tIdx - 1) match {
        case (p, t) if p < 0 || t < 0 => p
        case (p, t) if pattern(p) == text(t) => findLastMatchingPatternIndex(p, t)
        case (p, t) => p
      }

      if (textLength - offset < patternLength) acc
      else {
        findLastMatchingPatternIndex(maxPatternIndex, offset + maxPatternIndex) match {
          case pi if pi < 0 => if (stopAtFirstMatch) Seq(offset) else searchRecursive(offset + patternLength, acc :+ offset)
          case pi => searchRecursive(offset + shiftBadCharacterMap(text(offset + pi)) - (maxPatternIndex - pi), acc)
        }
      }
    }

    if (maxPatternIndex < 0 || maxTextIndex < 0) Seq.empty[Int]
    else searchRecursive(0, Seq.empty[Int])
  }

  private def createBadCharacterShift(pattern: String, numberOfCharacters: Int) : Array[Int] = {
    val patternLength = pattern.length
    val result = Array.fill[Int](numberOfCharacters)(patternLength)
    for (i <- 0 to patternLength - 2) result(pattern(i)) = patternLength - 1 - i
    result
  }
}
