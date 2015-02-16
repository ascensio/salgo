package geometry

import org.scalaalgo.geometry.Vector3D
import org.scalatest.{FunSuite, Matchers}

class QuickHullSpec extends FunSuite with Matchers {
  //test("Quick") { QuickHull.calculate(getPoints()) shouldEqual getPoints()}

  private def getPoints() : Array[Vector3D] = {
    /*Array (
      Vector3D(2, 5, 0),
      Vector3D(7, 1, 0),
      Vector3D(5, 6, 0))*/

    Array (
      Vector3D(2, 5, 0),
      Vector3D(7, 5, 0),
      Vector3D(5, 7, 0))
    }
}
