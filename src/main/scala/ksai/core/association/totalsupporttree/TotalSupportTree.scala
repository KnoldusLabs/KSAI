package ksai.core.association.totalsupporttree

import ksai.core.association.fptree.ItemSet

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class TotalSupportTree(
                             /**
                               * The index of items after sorting.
                               */
                             order: Array[Int],

                             /**
                               * The required minimum support.
                               */
                             minSupport: Int,

                             /**
                               * The root of T-Tree.
                               */
                             root: Node = Node()) {

  /**
    * Adds an itemset to the T-Tree with it's support value.
    *
    * @param itemset the given itemset, in which all the nodes must be
    *                arranged in their frequency descending order.
    * @param support the support value associated with the given itemset.
    */
  def add(itemset: Array[Int], support: Int): Unit = add(root, 0, itemset, itemset.length - 1, support)

  /**
    * Inserts a node into the T-Tree.
    *
    * @param node    the root of the subtree.
    * @param size    the size of the current array in T-Tree.
    * @param itemset the given itemset.
    * @param index   the current index of the given itemset.
    * @param support the support value associated with the given itemset.
    */
  @tailrec
  final def add(node: Node, size: Int, itemset: Array[Int], index: Int, support: Int): Unit = {
    node.maybeChildren = Option(node.maybeChildren.fold(new Array[Node](size))(identity))

    val item = order(itemset(index))

    node.maybeChildren = node.maybeChildren.fold[Option[Array[Node]]](None) { children =>
      children(item) = Option(children(item)).fold(Node(itemset(index)))(identity)
      Option(children)
    }

    if (index == 0) {
      node.maybeChildren.get(item).support += support
    } else {
      add(node.maybeChildren.get(item), item, itemset, index - 1, support)
    }
  }

  /**
    * Returns the support value of the given itemset.
    *
    * @param itemset the given itemset. the items in the itemset have to be
    *                in the descending order of their frequency.
    */
  def getSupport(itemset: Array[Int]): Int = getSupport(itemset, itemset.length - 1, root)

  /**
    * Returns the support value of the given itemset if found in the
    * T-Tree and 0 otherwise.
    *
    * @param itemset the given itemset.
    * @param index   the current index in the given itemset.
    * @param node    the node of the current T-Tree level.
    */
  def getSupport(itemset: Array[Int], index: Int, node: Node): Int = {
    val item = order(itemset(index))

    node.maybeChildren.fold(0) { children =>
      val maybeChild = Option(children(item))

      maybeChild.fold(0) { child =>
        if (index == 0) child.support
        else getSupport(itemset, index - 1, child)
      }
    }
  }

  /**
    * Mines the frequent itemsets. The discovered frequent itemsets
    * will be returned in a list.
    *
    * @return the list of frequent itemsets.
    */
  def getFrequentItemsets: ArrayBuffer[ItemSet] = {
    val list = new ArrayBuffer[ItemSet]()

    getFrequentItemsets(list)

    list
  }

  /**
    * Mines the frequent itemsets.
    *
    * @param list a container to store the frequent itemsets.
    * @return number of discovered frequent itemsets.
    */
  def getFrequentItemsets(list: ArrayBuffer[ItemSet]): Long = {
    var n: Long = 0

    if (root.maybeChildren.isDefined) {
      val children = root.maybeChildren.get

      var i = 0

      while (i < children.length) {
        val maybeChild = Option(children(i))

        if (maybeChild.isDefined && maybeChild.get.support >= minSupport) {
          val child = maybeChild.get

          n += getFrequentItemsets(list, child, i, Array(child.id))
        }

        i += 1
      }
    }

    n
  }

  /**
    * Mines the frequent itemsets.
    *
    * @param list    container to hold the frequent itemsets.
    * @param node    the node being checked if frequent or not.
    * @param size    the size of the current array of the T-Tree.
    * @param itemset the frequent itemset generated so far.
    * @return number of discovered frequent itemsets.
    */
  def getFrequentItemsets(list: ArrayBuffer[ItemSet], node: Node, size: Int, itemset: Array[Int]): Long = {
    val set = ItemSet(itemset, node.support)

    list += set

    var n: Long = 1

    if (node.maybeChildren.isDefined) {
      val children = node.maybeChildren.get

      var i = 0
      while (i < children.length) {
        val maybeChild = Option(children(i))

        if (maybeChild.isDefined && maybeChild.get.support >= minSupport) {
          val child = maybeChild.get
          val newItemset = child.id +: itemset
          n += getFrequentItemsets(list, child, i, newItemset)
        }

        i += 1
      }
    }

    n
  }
}

object TotalSupportTree {

  /**
    * Constructor for TotalSupportTree.
    *
    * @param order        the index of items after sorting.
    * @param minSupport   the required minimum support.
    * @param numFreqItems number of items with sufficient support, i.e.,
    *                     total number of frequent items.
    */
  def apply(order: Array[Int], minSupport: Int, numFreqItems: Int): TotalSupportTree = {
    val root = Node()
    root.maybeChildren = Option(root.maybeChildren.fold(new Array[Node](numFreqItems))(identity))

    new TotalSupportTree(order, minSupport, root)
  }
}
