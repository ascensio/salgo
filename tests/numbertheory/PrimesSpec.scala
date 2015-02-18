package numbertheory

import org.salgo.numbertheory.Primes
import org.scalatest.{FunSuite, Matchers}

class PrimesSpec extends FunSuite with Matchers {
  test("Get primes") { Primes.getPrimes(30) shouldEqual Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113) }
  test("Get primes by sundaram") { Primes.getPrimesBySundaram(30) shouldEqual Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113) }
  test("Get primes smaller than") { Primes.getPrimesSmallerThan(30) shouldEqual Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29) }
}
