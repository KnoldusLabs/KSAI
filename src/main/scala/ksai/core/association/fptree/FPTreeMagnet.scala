package ksai.core.association.fptree

import java.util

sealed trait FPTreeMagnet {
  type Result = FPTree

  def apply(): Result
}

object FPTreeMagnet {

  /**
    * FP-Tree Constructor. This is two-step construction, where user will call this
    * constructor and then add itemSets to the FP-Tree by calling [[FPTree.add]].
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
    * @param tuple the arguments of the constructor - itemSet and minimum support.
    */
  implicit def for2DArray(tuple: (Array[Array[Int]], Int)) = new FPTreeMagnet {
    def apply(): Result = {
      val (itemSets, minSupport) = (tuple._1, tuple._2)
      val frequencies = freq(itemSets)
      val fPTree = applyHelper(frequencies, minSupport)

      itemSets.foreach(fPTree.add)

      fPTree
    }
  }

  /**
    * Calculates frequency of the items in the input itemSets and stores them at
    * the item's value as an index in the new array.
    *
    * @param itemSets the itemSet containing items to calculate the frequency of.
    * @return the frequency of single items.
    */
  private def freq(itemSets: Array[Array[Int]]): Array[Int] = {
    var max = itemSets(0)(0)

    var i = 0
    while (i < itemSets.length) {
      var j = 0
      while (j < itemSets(i).length) {
        if (max < itemSets(i)(j)) max = itemSets(i)(j)
        j += 1
      }
      i += 1
    }

    val frequencies = new Array[Int](max + 1)

    i = 0
    while (i < itemSets.length) {
      var j = 0
      while (j < itemSets(i).length) {
        frequencies(itemSets(i)(j)) += 1
        j += 1
      }
      i += 1
    }

    frequencies
  }

  /**
    * Helper function for the construction of the FP-Tree object.
    *
    * @param frequencies frequency of each item in the itemSets.
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
        //i is the index at which it's frequency is stored. So i is basically the item in itemSet.
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
