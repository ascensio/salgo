package sequence.subsequences

import org.salgo.sequences.subsequences.Kadane
import org.scalatest.{FunSuite, Matchers}

class KadaneSpec extends FunSuite with Matchers {
  test("Empty sequence") { Kadane.findMaximumIntSubArray(Seq[Int]()) shouldEqual (0, Seq()) }
  test("One element sequence") { Kadane.findMaximumIntSubArray(Seq[Int](1)) shouldEqual (1, Seq(1)) }
  test("Simple sequence") { Kadane.findMaximumIntSubArray(Seq[Int](1,2,-4,1,4)) shouldEqual (5, Seq(1,4)) }
  test("Long sequence") { Kadane.findMaximumIntSubArray(Seq[Int](-2,1,-3,4,-1,2,1,-5,4)) shouldEqual (6, Seq(4,-1,2,1)) }
}
