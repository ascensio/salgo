package sorting

import org.salgo.sorting.{GeneralSortingAlgorithm, ShellSort}

class ShellSortSpec extends GenericSortSpec {
  override def getSortingAlgorithm: GeneralSortingAlgorithm = ShellSort
}
