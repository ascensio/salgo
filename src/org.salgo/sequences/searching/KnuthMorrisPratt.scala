package org.salgo.sequences.searching

import scala.annotation.tailrec

object KnuthMorrisPratt extends StringSearchAlgorithm {
  override def search(pattern: String, text: String, stopAtFirstMatch: Boolean, numberOfCharacters: Int): Seq[Int] = {
    val patternLength = pattern.length
    val textLength = text.length
    lazy val lookup = this.buildLookup(pattern)

    @tailrec
    def searchRecursive(offset: Int, index: Int, acc: Seq[Int]): Seq[Int] = {
      @tailrec
      def findMatchingPatternIndex(pIdx: Int, tIdx: Int): (Boolean, Int) = (pIdx + 1, tIdx + 1) match {
        case (p, t) if p >= patternLength || t >= textLength => (false, p)
        case (p, t) if p == patternLength - 1 => (true, p)
        case (p, t) if pattern(p) == text(t) => findMatchingPatternIndex(p, t)
        case (p, t) => (false, p)
      }

      if (offset + index >= textLength) acc
      else {
        val (found, patternMatchIndex) = findMatchingPatternIndex(index, offset)
        (found, patternMatchIndex) match {
          case (true, pi) => if (stopAtFirstMatch) Seq(offset) else searchRecursive(offset + pattern.length, 0, acc :+ offset)
          case (false, pi) =>
            val lookupValue = lookup(pi)
            if (lookupValue > -1) searchRecursive(offset + pi - lookupValue, lookupValue, acc)
            else searchRecursive(offset + 1, 0, acc)
        }
      }
    }

    if (textLength > 0 && patternLength > 0) searchRecursive(0, 0, Seq.empty[Int])
    else Seq.empty[Int]
  }

  private def buildLookup(pattern: String) : Array[Int] = {
    val patternLength = pattern.length

    @tailrec
    def buildLookupRecursive(cnd: Int, pos: Int, lookup: Array[Int]) : Array[Int] = (cnd, pos) match {
      case (c, p) if p >= patternLength => lookup
      case (c, p) if pattern(p - 1) == pattern(c) =>
        lookup(pos) = cnd
        buildLookupRecursive(cnd + 1, pos + 1, lookup)
      case (c, p) if c > 0 =>
        buildLookupRecursive(lookup(c), p, lookup)
      case (c, p) =>
        lookup(p) = 0
        buildLookupRecursive(c, p + 1, lookup)
    }

    buildLookupRecursive(0, 2, Array.fill[Int](pattern.length)(0))
  }
}
