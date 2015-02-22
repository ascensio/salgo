package geometry.hulls

import org.salgo.geometry.hulls.{GiftWrapping, HullScan2D}

class GiftWrappingSpec  extends GenericHullScan2DSpec {
  override def getHullAlgorithm: HullScan2D = GiftWrapping
}
