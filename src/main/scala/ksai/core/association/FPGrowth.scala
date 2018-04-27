package ksai.core.association

import java.io.PrintStream

import scala.collection.mutable.ArrayBuffer

class FPGrowth(minSupport: Int, val T0: FPTree) {

  @volatile
  var list: ArrayBuffer[ItemSet] = new ArrayBuffer[ItemSet]()

  def learn(): ArrayBuffer[ItemSet] = {
    learn(None)

    list
  }

  def learn(maybeOut: Option[PrintStream]): Long = {
    grow(maybeOut, T0, None)
  }

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

    // implement total support tree

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

          //implement total support tree

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
}

object FPGrowth {

  def apply(magnet: FPGrowthMagnet): magnet.Result = magnet()
}
