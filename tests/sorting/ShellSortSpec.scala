package sorting

import org.scalaalgo.sorting.{GeneralSortingAlgorithm, ShellSort}

class ShellSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = ShellSort
}
