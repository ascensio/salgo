package numbertheory

import org.salgo.numbertheory.GreatestCommonDivisor
import org.scalatest.{FunSuite, Matchers}

class GreatestCommonDivisorSpec extends FunSuite with Matchers {
  test("Euclid (first small)") { GreatestCommonDivisor.getByEuclid(20, 10) shouldEqual 10 }
  test("Euclid (second small)") { GreatestCommonDivisor.getByEuclid(10, 20) shouldEqual 10 }
  test("Euclid (multi level)") { GreatestCommonDivisor.getByEuclid(25, 10) shouldEqual 5 }
  test("Euclid (no divisor)") { GreatestCommonDivisor.getByEuclid(25, 13) shouldEqual 1 }

  test("GDC (first small)") { GreatestCommonDivisor.getByBinary(20, 10) shouldEqual 10 }
  test("GDC (second small)") { GreatestCommonDivisor.getByBinary(10, 20) shouldEqual 10 }
  test("GDC (multi level)") { GreatestCommonDivisor.getByBinary(25, 10) shouldEqual 5 }
  test("GDC (no divisor)") { GreatestCommonDivisor.getByBinary(25, 13) shouldEqual 1 }
}
