package geometry.hulls

import org.salgo.geometry.hulls.GrahamScan

class GrahamScanSpec extends GenericHullScan2DSpec {
  override def getHullAlgorithm = GrahamScan
}
