package ksai.core.association.fptree

import java.io.PrintStream

import ksai.core.association.totalsupporttree.TotalSupportTree

import scala.collection.mutable.ArrayBuffer

class FPGrowth(
                /**
                  * The required minimum support of the itemsets.
                  */
                minSupport: Int,

                /**
                  * The FP-Tree
                  */
                val T0: FPTree) {

  /**
    * The list of frequent itemsets.
    */
  var list: ArrayBuffer[ItemSet] = new ArrayBuffer[ItemSet]()

  /**
    * The total support tree.
    */
  var maybeTtree: Option[TotalSupportTree] = None

  /**
    * Mines the frequent itemsets. These itemsets are put in the [[list]]
    * and returned.
    */
  def learn(): ArrayBuffer[ItemSet] = {
    learn(None)

    list
  }

  /**
    * Mines the frequent itemsets. These itemsets are printed out
    * to the provided stream.
    *
    * @param maybeOut the provided stream.
    */
  def learn(maybeOut: Option[PrintStream]): Long = {
    grow(maybeOut, T0, None)
  }

  /**
    * Mines the frequent itemsets. Start with the bottom of the header table and
    * work upwards. For each available FP-Tree Node -
    * <OL>
    * <LI> Count the support.</LI>
    * <LI> Build up itemset so far.</LI>
    * <LI> Add to supported sets.</LI>
    * <LI> Build a new FP-Tree: (i) Create a new local root, (ii) Create a new
    * header table, and (iii) populate with ancestors.</LI>
    * <LI> If new local FP-Tree is not empty, repeat the mining operation.</LI>
    * </OL>
    * Otherwise end.
    *
    * @param maybeOut     the provided output stream.
    * @param fPTree       the FP-Tree to mine from.
    * @param maybeItemset the current itemset as generated so far.
    */
  def grow(maybeOut: Option[PrintStream],
           fPTree: FPTree,
           maybeItemset: Option[Array[Int]]): Long = {
    var n: Long = 0

    val localItemSet = new Array[Int](T0.numItems)
    val prefixItemSet = new Array[Int](T0.maxItemSetSize)

    fPTree.headerTable.reverse.foreach { headerTableItem =>

      n += grow(headerTableItem,
        maybeItemset,
        maybeOut,
        localItemSet,
        prefixItemSet)

    }

    n
  }

  /**
    * Mines FP-Tree with respect to a single item in the header table.
    *
    * @param headerTableItem  the header table item to mine for.
    * @param maybeItemset     the current itemset as generated so far.
    * @param maybeOut         the provided output stream.
    * @param localItemSupport support of nodes in conditional FP-Tree.
    * @param prefixItemSet    the nodes to be added in the conditional FP-Tree.
    */
  def grow(headerTableItem: HeaderTableItem,
           maybeItemset: Option[Array[Int]],
           maybeOut: Option[PrintStream],
           localItemSupport: Array[Int],
           prefixItemSet: Array[Int]): Long = {

    var n: Long = 1

    val support = headerTableItem.count

    val item = headerTableItem.id

    val itemset = maybeItemset.fold(Array(item)) { x =>
      val n = x.length + 1
      val newItemset = new Array[Int](n)
      newItemset(0) = item
      java.lang.System.arraycopy(x, 0, newItemset, 1, n - 1)

      newItemset
    }

    list += ItemSet(itemset, support)

    maybeTtree = maybeTtree.fold[Option[TotalSupportTree]](None) { ttree =>
      ttree.add(itemset, support)
      Some(ttree)
    }


    val maybeHeaderTableItemNode: Option[Node] = headerTableItem.maybeNode.fold[Option[Node]](None)(node => node.next)

    if (maybeHeaderTableItemNode.isEmpty) {
      var maybeNode = headerTableItem.maybeNode

      while (maybeNode.isDefined) {
        val node = maybeNode.get
        var maybeParent = node.parent

        var newItemset = itemset

        while (maybeParent.isDefined) {
          val parent = maybeParent.get

          n += 1
          newItemset = parent.id +: newItemset

          list += ItemSet(newItemset, support)

          maybeTtree = maybeTtree.fold[Option[TotalSupportTree]](None) { ttree =>
            ttree.add(newItemset, support)
            Some(ttree)
          }

          maybeParent = parent.parent
        }

        maybeNode = node.parent
      }
    } else {
      val newLocalItemSupport: Array[Int] = getLocalItemSupport(headerTableItem.maybeNode, localItemSupport)
      if (newLocalItemSupport.nonEmpty) {

        val fPTree = getLocalFPTree(headerTableItem.maybeNode, newLocalItemSupport, prefixItemSet)

        n += grow(Some(itemset),
          maybeOut,
          localItemSupport,
          prefixItemSet,
          fPTree)
      }
    }

    n
  }

  /**
    * Mines the frequent itemsets of a conditional FP-Tree by traversing the
    * header table from bottom towards top.
    *
    * @param maybeItemset     the current itemset as generated so far.
    * @param maybeOut         the provided output stream.
    * @param localItemSupport support of nodes in conditional FP-Tree.
    * @param prefixItemSet    the nodes to be added in the conditional FP-Tree.
    * @param fPTree           the conditional FP-Tree.
    */
  def grow(maybeItemset: Option[Array[Int]],
           maybeOut: Option[PrintStream],
           localItemSupport: Array[Int],
           prefixItemSet: Array[Int],
           fPTree: FPTree): Long = {

    var n: Long = 0

    fPTree.headerTable.reverse.foreach { headerTableItem =>
      n += grow(headerTableItem,
        maybeItemset,
        maybeOut,
        localItemSupport,
        prefixItemSet)
    }

    n
  }

  /**
    * Counts the supports of single items in ancestor item sets linked list.
    *
    * @param maybeNode        the node whose ancestor's supports need to be counted.
    * @param localItemSupport the support of the node's ancestors.
    * @return the counted support of all the ancestors of the node.
    */
  def getLocalItemSupport(maybeNode: Option[Node], localItemSupport: Array[Int]): Array[Int] = {

    val mutableLocalItemSupport = Array.fill(localItemSupport.length)(0)
    var mutableMaybeNode = maybeNode
    var end = true

    while (mutableMaybeNode.isDefined) {
      val node = mutableMaybeNode.get
      var maybeParent = node.parent
      val support = node.count

      while (maybeParent.isDefined) {
        val parent = maybeParent.get
        mutableLocalItemSupport(parent.id) += support
        end = false

        maybeParent = parent.parent
      }

      mutableMaybeNode = node.next
    }

    if (!end) mutableLocalItemSupport else new Array[Int](0)
  }

  /**
    * Generates a conditional FP-Tree.
    *
    * @param maybeNode        the node for which the conditional FP-Tree needs to be built.
    * @param localItemSupport the supports of the ancestors of the node.
    * @param prefixItemSet    the ancestors of the node that need to be added in the
    *                         conditional FP-Tree.
    */
  def getLocalFPTree(maybeNode: Option[Node], localItemSupport: Array[Int], prefixItemSet: Array[Int]): FPTree = {
    val tree = FPTree(localItemSupport, minSupport)

    var mutableMaybeNode = maybeNode
    while (mutableMaybeNode.isDefined) {
      val node = mutableMaybeNode.get
      var maybeParent = node.parent
      var i = prefixItemSet.length

      while (maybeParent.isDefined) {
        val parent = maybeParent.get
        if (localItemSupport(parent.id) >= minSupport) {
          i -= 1
          prefixItemSet(i) = parent.id
        }
        maybeParent = parent.parent
      }

      if (i < prefixItemSet.length) {
        tree.add(i, prefixItemSet.length, prefixItemSet, node.count)
      }

      mutableMaybeNode = node.next
    }

    tree
  }

  /**
    * Add an itemset into the FP-Tree.
    *
    * @param itemset an itemset, that should NOT contain duplicated items.
    *                Note that it is reordered after the call.
    */
  def add(itemset: Array[Int]): Unit = T0.add(itemset)

  /**
    * Returns the number of transactions in the FP-Tree.
    */
  def size: Int = T0.size

  /**
    * Mines the frequent itemsets. Discovered frequent itemsets are stored in a
    * total support tree.
    */
  def buildTotalSupportTree(): Option[TotalSupportTree] = {
    maybeTtree = Some(TotalSupportTree(T0.order, minSupport, T0.numFreqItems))
    learn()
    maybeTtree
  }
}

object FPGrowth {

  def apply(magnet: FPGrowthMagnet): magnet.Result = magnet()
}
