package geometry.structures

import org.scalaalgo.geometry.structures.Vector2D
import org.scalatest.{FunSuite, Matchers}

class Vector2DSpec extends FunSuite with Matchers {
  test("Angle of 0 degrees") { Vector2D.getAngleInDegrees(Vector2D(0,1), Vector2D(0,1)) shouldEqual 0.0}
  test("Angle of 90 degrees") { Vector2D.getAngleInDegrees(Vector2D(0,1), Vector2D(1,0)) shouldEqual 90.0}
  test("Angle of 180 degrees") { Vector2D.getAngleInDegrees(Vector2D(0,1), Vector2D(0,-1)) shouldEqual 180.0 }
  test("Angle of 270 degrees") { Vector2D.getAngleInDegrees(Vector2D(0,1), Vector2D(-1,0)) shouldEqual 270.0}
}