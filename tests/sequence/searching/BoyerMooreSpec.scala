package sequence.searching

import org.salgo.sequences.searching.BoyerMoore
import org.scalatest.{FunSuite, Matchers}

class BoyerMooreSpec extends FunSuite with Matchers {

  test("Full match") { BoyerMoore.search("test", "test") shouldEqual Seq(0) }
  test("Start match") { BoyerMoore.search("test", "testtext") shouldEqual Seq(0) }
  test("Start + 1 match") { BoyerMoore.search("test", "atest") shouldEqual Seq(1) }
  test("Start + 4 match") { BoyerMoore.search("test", "texttest") shouldEqual Seq(4) }
  test("Middle match") { BoyerMoore.search("test", "texttesttext") shouldEqual Seq(4) }
  test("Middle match in complex sequence") { BoyerMoore.search("abcabba", "abaabcabbababbcababa") shouldEqual Seq(3) }

  test("Double full match") { BoyerMoore.search("test", "testtest") shouldEqual Seq(0, 4) }
  test("Double start and end match") { BoyerMoore.search("test", "testtexttest") shouldEqual Seq(0, 8) }
  test("Double start + 1 and end - 1 match") { BoyerMoore.search("test", "atesttesta") shouldEqual Seq(1, 5) }
  test("Double start + 4 and end - 4 match") { BoyerMoore.search("test", "texttesttesttext") shouldEqual Seq(4, 8) }
  test("Double middle match with multi delimiter") { BoyerMoore.search("test", "texttesttexttesttext") shouldEqual Seq(4, 12) }
  test("Double middle match with single delimiter") { BoyerMoore.search("test", "texttestatesttext") shouldEqual Seq(4, 9) }
  test("Double middle match in complex sequence") { BoyerMoore.search("abcabba", "abaabcabbababbcabababcabbaa") shouldEqual Seq(3, 19) }
}