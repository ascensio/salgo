package sequence.searching

import org.salgo.sequences.searching.StringSearchAlgorithm
import org.scalatest.{FunSuite, Matchers}

abstract class StringSearchAlgorithmSpec extends FunSuite with Matchers {
  test("Empty text") { this.getAlgorithm.search("test", "") shouldEqual Seq() }
  test("Empty pattern") { this.getAlgorithm.search("", "test") shouldEqual Seq() }
  test("Full match") { this.getAlgorithm.search("test", "test") shouldEqual Seq(0) }
  test("Start match") { this.getAlgorithm.search("test", "testtext") shouldEqual Seq(0) }
  test("Start + 1 match") { this.getAlgorithm.search("test", "atest") shouldEqual Seq(1) }
  test("Start + 4 match") { this.getAlgorithm.search("test", "texttest") shouldEqual Seq(4) }
  test("Middle match") { this.getAlgorithm.search("test", "texttesttext") shouldEqual Seq(4) }
  test("Middle match in complex sequence") { this.getAlgorithm.search("abcabba", "abaabcabbababbcababa") shouldEqual Seq(3) }

  test("Double full match") { this.getAlgorithm.search("test", "testtest") shouldEqual Seq(0, 4) }
  test("Double start and end match") { this.getAlgorithm.search("test", "testtexttest") shouldEqual Seq(0, 8) }
  test("Double start + 1 and end - 1 match") { this.getAlgorithm.search("test", "atesttesta") shouldEqual Seq(1, 5) }
  test("Double start + 4 and end - 4 match") { this.getAlgorithm.search("test", "texttesttesttext") shouldEqual Seq(4, 8) }
  test("Double middle match with multi delimiter") { this.getAlgorithm.search("test", "texttesttexttesttext") shouldEqual Seq(4, 12) }
  test("Double middle match with single delimiter") { this.getAlgorithm.search("test", "texttestatesttext") shouldEqual Seq(4, 9) }
  test("Double middle match in complex sequence") { this.getAlgorithm.search("abcabba", "abaabcabbababbcabababcabbaa") shouldEqual Seq(3, 19) }

  def getAlgorithm: StringSearchAlgorithm
}
