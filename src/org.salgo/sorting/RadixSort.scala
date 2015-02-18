package org.salgo.sorting

object RadixSort extends IntegerSortingAlgorithm {

  override def sort(seq: Array[Int])(implicit ev: (Int) => Ordered[Int]): Unit = {
    val length = seq.length
    if (length > 1){
      val base = 10
      val max = seq.max
      val buckets = scala.collection.mutable.LinearSeq.fill[List[Int]](base)(List[Int]())

      var n = 1
      while (n <= max) {
        for (i <- 0 to length - 1) {
          val currentValue = seq(i)
          val bucketIndex = (currentValue / n) % base
          val bucket = buckets(bucketIndex)
          buckets(bucketIndex) = currentValue :: bucket
        }

        var c = 0
        for (i <- 0 to base - 1) {
          val bucket = buckets(i)
          for (j <- 0 to bucket.length - 1) {
            seq(c) = bucket(j)
            c += 1
          }
          buckets(i) = List[Int]()
        }

        n *= base
      }
    }
  }
}
