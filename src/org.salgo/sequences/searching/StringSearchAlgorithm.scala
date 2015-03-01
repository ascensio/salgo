package org.salgo.sequences.searching

trait StringSearchAlgorithm {
  def search(pattern: String, text: String, stopAtFirstMatch: Boolean = false, numberOfCharacters: Int = 256) : Seq[Int]
}
