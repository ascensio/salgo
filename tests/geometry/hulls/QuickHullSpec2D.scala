package geometry.hulls

import org.salgo.geometry.hulls.{HullScan2D, QuickHull2D}

class QuickHullSpec2D   extends GenericHullScan2DSpec {
  override def getHullAlgorithm: HullScan2D = QuickHull2D
}


