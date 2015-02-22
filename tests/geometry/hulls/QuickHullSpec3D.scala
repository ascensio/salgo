package geometry.hulls

import org.salgo.geometry.hulls.QuickHull3D
import org.salgo.geometry.structures.Point3D
import org.scalatest.{FunSuite, Matchers}

class QuickHullSpec3D extends FunSuite with Matchers {
  test("Hull 3D with no points") { QuickHull3D.solve(Traversable.empty[Point3D]) should have size 0 }
  test("Hull 3D with one points") { QuickHull3D.solve(Seq(Point3D(1,1,1))) should contain only Point3D(1,1,1) }
  test("Hull 3D with two points") { QuickHull3D.solve(Seq(Point3D(1,1,1), Point3D(1,2,3))) should contain theSameElementsAs Seq(Point3D(1,1,1), Point3D(1,2,3)) }
  test("Hull 3D with three points") { QuickHull3D.solve(Seq(Point3D(1,1,1), Point3D(1,2,3), Point3D(2,4,7))) should contain theSameElementsAs Seq(Point3D(1,1,1), Point3D(1,2,3), Point3D(2,4,7)) }
  test("Hull 3D for simple cloud") { QuickHull3D.solve(this.getPointsForSimpleCloud) should contain theSameElementsAs this.getExpectedPointsForSimpleCloud}
  test("Hull 3D for simple cloud with border point") { QuickHull3D.solve(this.getPointsForSimpleCloudWithBorderPoint) should contain theSameElementsAs this.getExpectedPointsForSimpleCloud}
  test("Hull 3D for complex cloud") { QuickHull3D.solve(this.getPointsForComplexCloud) should contain theSameElementsAs this.getExpectedPointsForComplexCloud}

  private def getPointsForSimpleCloud: Traversable[Point3D] = {
    Point3D((5,0,0),(0,-5,0),(-5,0,0),(0,0,4),(0,5,0),(0,0,5),(0,0,-5))
  }

  private def getPointsForSimpleCloudWithBorderPoint: Traversable[Point3D] = {
    Point3D((5,0,0),(0,-5,0),(-5,0,0),(0,0,4),(0,5,0),(0,0,5),(0,0,-5))
  }

  private def getExpectedPointsForSimpleCloud: Traversable[Point3D] = {
    Point3D((5,0,0),(0,-5,0),(-5,0,0),(0,5,0),(0,0,5),(0,0,-5))
  }

  private def getPointsForComplexCloud: Traversable[Point3D] = {
    Point3D((5,0,0),(0,-5,0),(2,1,-1),(-5,0,0),(1,1,1),(0,5,0),(-2,1,1),(0,0,5),(0,0,4),(0,0,-5))
  }

  private def getExpectedPointsForComplexCloud: Traversable[Point3D] = {
    Point3D((5,0,0),(0,-5,0),(-5,0,0),(0,5,0),(0,0,5),(0,0,-5))
  }
}

