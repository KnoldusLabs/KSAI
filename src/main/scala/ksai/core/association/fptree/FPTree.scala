package ksai.core.association.fptree

final class FPTree(
                    /**
                      * The required minimum support of the itemsets.
                      */
                    minSupport: Int,

                    /**
                      * The support of single items.
                      */
                    itemSupport: Array[Int],

                    /**
                      * Header table.
                      */
                    var headerTable: Array[HeaderTableItem],

                    /**
                      * The order of items according to their support.
                      */
                    val order: Array[Int],

                    /**
                      * The root node of the FP-Tree.
                      */
                    root: Node,

                    /**
                      * The number of transactions in the database.
                      */
                    var numTransactions: Int = 0,

                    /**
                      * The number of sigle items in the database.
                      */
                    val numItems: Int = 0,

                    /**
                      * The number of frequent items with sufficient support.
                      */
                    val numFreqItems: Int = 0,

                    /**
                      * The size of largest item set (with only frequent items) in the database.
                      */
                    var maxItemSetSize: Int = -1) {

  /**
    * Add an itemset into the FP-Tree.
    *
    * @param itemset an itemset which should NOT contain duplicate items.
    */
  def add(itemset: Array[Int]): Unit = {
    numTransactions += 1
    var m = 0

    val o = itemset.map { item =>
      if (itemSupport(item) >= minSupport) m += 1
      order(item)
    }

    if (m > 0) {

      //quicksort
      var index = 0

      val zippedCollection = new Array[(Int, Int)](itemset.length)
      while (index < itemset.length) {
        zippedCollection(index) = (o(index), itemset(index))
        index += 1
      }

      val sortedCollection = zippedCollection.sortBy(_._1)

      index = 0

      val sortedItemset = new Array[Int](itemset.length)
      while (index < itemset.length) {
        sortedItemset(index) = sortedCollection(index)._2
        index += 1
      }

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

  /**
    * Add an itemset into the FP-Tree. The items in the itemset are already in the
    * descending order of their frequency.
    *
    * @param index   the current item index in the itemset.
    * @param end     the end index of the itemset.
    * @param itemset an itemset.
    * @param support the support associated with the itemset.
    */
  def add(index: Int, end: Int, itemset: Array[Int], support: Int): Unit = {
    root.add(index, end, itemset, support, this)
  }

  /**
    * Returns the number of transactions in the database.
    */
  def size: Int = numTransactions
}

object FPTree {

  def apply(magnet: FPTreeMagnet): magnet.Result = magnet()
}
