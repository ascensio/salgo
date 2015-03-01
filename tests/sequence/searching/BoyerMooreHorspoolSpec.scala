package sequence.searching

import org.salgo.sequences.searching.BoyerMooreHorspool

class BoyerMooreHorspoolSpec extends StringSearchAlgorithmSpec {
  override def getAlgorithm = BoyerMooreHorspool

  test("Middle match in complex sequence2") { this.getAlgorithm.search("abcabba", "abaabcabbababbcababa") shouldEqual Seq(3) }
}
