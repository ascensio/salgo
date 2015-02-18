package sequence.matching

import org.salgo.sequences.matching.Bitap
import org.scalatest.{FunSuite, Matchers}

class BitapSpec extends FunSuite with Matchers {
  test("Simple search") { Bitap.search("Peter", "te").get shouldEqual 3 }
  test("Search") { Bitap.search("Hello Peter this is Hans", "ter").get shouldEqual 10 }
  test("Search with multiple hits") { Bitap.search("Hello Peter this is Hans", "is").get shouldEqual 15 }
  test("Search with no hits") { Bitap.search("Hello Peter this is Hans", "ttt") shouldEqual None }
}
