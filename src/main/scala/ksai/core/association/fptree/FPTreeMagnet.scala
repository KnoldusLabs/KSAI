package ksai.core.association.fptree

import java.util

sealed trait FPTreeMagnet {
  type Result = FPTree

  def apply(): Result
}

object FPTreeMagnet {

  /**
    * FP-Tree Constructor. This is two-step construction, where user will call this
    * constructor and then add itemsets to the FP-Tree by calling [[FPTree.add]].
    *
    * @param tuple the arguments of the constructor - frequencies of each item and
    *              minimum support.
    */
  implicit def for1DArray(tuple: (Array[Int], Int)) = new FPTreeMagnet {
    val (frequencies, minSupport) = (tuple._1, tuple._2)

    def apply(): Result = applyHelper(frequencies, minSupport)
  }

  /**
    * FP-Tree Constructor. This is one-step construction.
    *
    * @param tuple the arguments of the constructor - itemset and minimum support.
    */
  implicit def for2DArray(tuple: (Array[Array[Int]], Int)) = new FPTreeMagnet {
    def apply(): Result = {
      val (itemsets, minSupport) = (tuple._1, tuple._2)
      val frequencies = freq(itemsets)
      val fPTree = applyHelper(frequencies, minSupport)

      itemsets.foreach(fPTree.add)

      fPTree
    }
  }

  /**
    * Calculates frequency of the items in the input itemsets and stores them at
    * the item's value as an index in the new array.
    *
    * @param itemsets the itemset containing items to calculate the frequency of.
    * @return the frequency of single items.
    */
  private def freq(itemsets: Array[Array[Int]]): Array[Int] = {
    var max = itemsets(0)(0)

    var i = 0
    while (i < itemsets.length) {
      var j = 0
      while (j < itemsets(i).length) {
        if (max < itemsets(i)(j)) max = itemsets(i)(j)
        j += 1
      }
      i += 1
    }

    val frequencies = new Array[Int](max + 1)

    i = 0
    while (i < itemsets.length) {
      var j = 0
      while (j < itemsets(i).length) {
        frequencies(itemsets(i)(j)) += 1
        j += 1
      }
      i += 1
    }

    frequencies
  }

  /**
    * Helper function for the construction of the FP-Tree object.
    *
    * @param frequencies frequency of each item in the itemsets.
    * @param minSupport  the required minimum support of item sets in terms of
    *                    frequency.
    */
  private def applyHelper(frequencies: Array[Int], minSupport: Int): FPTree = {
    val root = new Node()
    val numItems = frequencies.length
    var numFreqItems = 0

    var i = 0

    while (i < frequencies.length) {
      if (frequencies(i) >= minSupport) {
        numFreqItems += 1
      }
      i += 1
    }

    val headerTable = new Array[HeaderTableItem](numFreqItems)
    i = 0
    var j = 0

    while (i < frequencies.length) {
      if (frequencies(i) >= minSupport) {
        //i is the index at which it's frequency is stored. So i is basically the item in itemset.
        headerTable(j) = new HeaderTableItem(i, count = frequencies(i))
        j += 1
      }
      i += 1
    }

    java.util.Arrays.sort(headerTable, HeaderTableItemComparator)

    val order = new Array[Int](numItems)
    util.Arrays.fill(order, numItems)

    i = 0

    while (i < numFreqItems) {
      //position of each item in header table is being stored in this array.
      order(headerTable(i).id) = i
      i += 1
    }

    new FPTree(
      minSupport,
      frequencies,
      headerTable,
      order,
      root,
      numItems = numItems,
      numFreqItems = numFreqItems)
  }
}
