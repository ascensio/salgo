package org.salgo.numbertheory

object Primes {
  def isPrime(number: Int) : Boolean = {
    number match {
      case 1 => false
      case 2 => true
      case n if n % 2 == 0 => false
      case n =>
        for(j <- 3 to math.sqrt(number).toInt by 1) if (number % j == 0) false
        true
    }
  }

  def getPrimes(numberOfPrimes: Int) : Seq[Int] = {
    val limit = this.getApproximatedIndexOfPrimeNumber(numberOfPrimes)
    val map = this.fillSieveOfEratosthenes(limit)

    var found = 0
    val result = Array.fill[Int](numberOfPrimes)(0)
    for(i <- 0 to limit by 1 if found < numberOfPrimes) {
      if (map(i)) {
        result.update(found, i)
        found += 1
      }
    }

    result
  }

  def getPrimesSmallerThan(limit: Int) : Seq[Int] = {
    val map = this.fillSieveOfEratosthenes(limit)
    var result = List[Int]()
    for(i <- 0 to limit by 1) {
      if (map(i)) result = result :+ i
    }

    result
  }

  private def fillSieveOfEratosthenes(limit: Int): Array[Boolean] = {
    val map = Array.fill[Boolean](limit + 1)(true)
    map(0) = false
    map(1) = false
    var i = 0
    while (i * i <= limit) {
      if (map(i)) {
        for (k <- i * i to limit by i) {
          map(k) = false
        }
      }
      i += 1
    }

    map
  }

  def getPrimesBySundaram(numberOfPrimes: Int) : Seq[Int] = {
    val limit = this.getApproximatedIndexOfPrimeNumber(numberOfPrimes)
    val map = this.fillSieveOfSundaram(limit)

    val result = Array.fill[Int](numberOfPrimes)(0)
    result(0) = 2
    var found = 1

    var i = 1
    while (2 * i + 1 <= limit) {
      if (found >= numberOfPrimes) return result

      if (map(i)) {
        result(found) = 2 * i + 1
        found += 1
      }

      i += 1
    }

    result
  }

  private def fillSieveOfSundaram(limit: Int): Array[Boolean] = {
    val halfLimit = limit / 2
    val map = Array.fill(halfLimit + 1)(true)
    var i = 1
    while (3 * i + 1 < halfLimit) {
      var j = 1
      while (i + j + 2 * i * j <= halfLimit) {
        map(i + j + 2 * i * j) = false
        j += 1
      }
      i += 1
    }

    map
  }

  def getApproximatedIndexOfPrimeNumber(primeNumber: Int) : Int = {
    primeNumber match {
      case p if p >= 7022 =>
        val pnd = primeNumber.toDouble
        (pnd * math.log(pnd) + pnd * (math.log(math.log(pnd)) - 0.9385)).toInt
      case p if p >= 6 =>
        val pnd = primeNumber.toDouble
        (pnd * math.log(pnd) + pnd * math.log(math.log(pnd))).toInt
      case 5 => 11
      case 4 => 7
      case 3 => 5
      case 2 => 3
      case 1 => 2
      case _ => 0
     }
  }
}
