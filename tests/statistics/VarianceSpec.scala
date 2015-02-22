package statistics

import org.salgo.statistics.Variance
import org.scalatest.{FunSuite, Matchers}

class VarianceSpec extends FunSuite with Matchers {
  test("Variance naive") { Variance.calculateNaive(this.getSimpleValues) shouldEqual 30}
  test("Variance online") { Variance.calculateOnline(this.getSimpleValues) shouldEqual 30}
  test("Variance shifted") { Variance.calculateShifted(this.getSimpleValues) shouldEqual 30}
  test("Variance two pass") { Variance.calculateTwoPass(this.getSimpleValues) shouldEqual 30}

  private def getSimpleValues: Traversable[Double] = {
    Seq(4, 7, 13, 16)
  }
}
