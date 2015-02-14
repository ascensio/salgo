package combinatorical

import org.scalaalgo.combinatorical.{CycleFinder, CycleInformation}
import org.scalatest.{FunSuite, Matchers}

abstract class CycleFinderSpec extends FunSuite with Matchers {
  test("Detect cycle") { getCycleFinder.findCycle(accessFunc, 8) shouldEqual CycleInformation(3, 3) }
  test("Detect one element cycle") { getCycleFinder.findCycle(oneElementCycleFunc, 6) shouldEqual CycleInformation(1, 0) }
  test("Detect ring cycle") { getCycleFinder.findCycle(ringElementCycleFunc, 8) shouldEqual CycleInformation(9, 0) }

  def getCycleFinder : CycleFinder

  private def accessFunc(x: Int) : Int = {
    x match {
      case 0 => 6
      case 1 => 6
      case 2 => 0
      case 3 => 1
      case 4 => 4
      case 5 => 3
      case 6 => 3
      case 7 => 4
      case 8 => 2
    }
  }

  private def oneElementCycleFunc(x: Int) : Int = {
    x match {
      case 0 => 6
      case 1 => 6
      case 2 => 6
      case 3 => 6
      case 4 => 6
      case 5 => 6
      case 6 => 6
      case 7 => 6
      case 8 => 6
    }
  }

  private def ringElementCycleFunc(x: Int) : Int = {
    x match {
      case 0 => 1
      case 1 => 2
      case 2 => 3
      case 3 => 4
      case 4 => 5
      case 5 => 6
      case 6 => 7
      case 7 => 8
      case 8 => 0
    }
  }
}
