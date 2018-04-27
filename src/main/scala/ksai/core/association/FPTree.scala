package ksai.core.association

final class FPTree(minSupport: Int,
                   itemSupport: Array[Int],
                   var headerTable: Array[HeaderTableItem],
                   val order: Array[Int],
                   root: Node,
                   var numTransactions: Int = 0,
                   val numItems: Int = 0,
                   val numFreqItems: Int = 0,
                   var maxItemSetSize: Int = -1) {

  def add(itemset: Array[Int]): Unit = {
    numTransactions += 1
    var m = 0

    val o = itemset.map { item =>
      if (itemSupport(item) >= minSupport) m += 1
      order(item)
    }

    if (m > 0) {

      //quicksort
      val zippedCollection = o zip itemset

      val sortedItemset = zippedCollection.sortBy(x => x._1).map(_._2)

      var i = 1
      while (i < m) {
        if (sortedItemset(i) == sortedItemset(i - 1)) {
          m -= 1
          var j = i
          while (j < m) {
            sortedItemset(j) = sortedItemset(j + 1)
            j += 1
          }
        }
        i += 1
      }

      root.add(0, m, sortedItemset, 1, this)
    }
  }

  def add(index: Int, end: Int, itemset: Array[Int], support: Int): Unit = {
    root.add(index, end, itemset, support, this)
  }
}

object FPTree {

  def apply(magnet: DimensionMagnet): magnet.Result = magnet()
}
