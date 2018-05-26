package ksai.core.association.fptree

import java.io.PrintStream

import akka.actor.{Actor, Props}
import ksai.core.association.totalsupporttree.TotalSupportTree

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class Grow(headerTableItem: HeaderTableItem,
                maybeItemSet: Option[Array[Int]],
                maybeOut: Option[PrintStream],
                localItemSet: Array[Int],
                prefixItemSet: Array[Int])

case object GetList

case class Rules(nrOfRules: Long, rules: ArrayBuffer[ItemSet])

class FPGrowthTask(minSupport: Int) extends Actor {

  /**
    * The list of frequent item sets.
    */
  var list: ArrayBuffer[ItemSet] = new ArrayBuffer[ItemSet]()

  override def receive: Receive = {

    case Grow(headerTableItem, maybeItemSet, maybeOut, localItemSet, prefixItemSet) =>
      val currentSender = sender()
      list = new ArrayBuffer[ItemSet]()
      currentSender ! Rules(grow(headerTableItem, maybeItemSet, maybeOut, localItemSet, prefixItemSet), list)
  }

  /**
    * Mines FP-Tree with respect to a single item in the header table.
    *
    * @param headerTableItem  the header table item to mine for.
    * @param maybeItemSet     the current item set as generated so far.
    * @param maybeOut         the provided output stream.
    * @param localItemSupport support of nodes in conditional FP-Tree.
    * @param prefixItemSet    the nodes to be added in the conditional FP-Tree.
    */
  def grow(headerTableItem: HeaderTableItem,
           maybeItemSet: Option[Array[Int]],
           maybeOut: Option[PrintStream],
           localItemSupport: Array[Int],
           prefixItemSet: Array[Int]): Long = {

    var n: Long = 1
    val support = headerTableItem.count

    val item = headerTableItem.id
    val itemSet = maybeItemSet.fold(Array(item))(x => item +: x)

    list += ItemSet(itemSet, support)

    val maybeHeaderTableItemNode: Option[Node] = headerTableItem.maybeNode.fold[Option[Node]](None)(node => node.next)

    if (maybeHeaderTableItemNode.isEmpty) {
      val maybeNode = headerTableItem.maybeNode

      @tailrec
      def iterateSameNodes(maybeNode: Option[Node], n: Long): Long = {
        if (maybeNode.isEmpty) {
          n
        } else {
          val maybeParent = maybeNode.fold[Option[Node]](None)(_.parent)

          @tailrec
          def iterateParentNodes(maybeParent: Option[Node], n: Long, itemSet: Array[Int]): Long =
            if (maybeParent.isEmpty) {
              n
            } else {
              val newItemSet = maybeParent.fold(itemSet) { parent =>
                parent.id +: itemSet
              }
              val newMaybeParent = maybeParent.fold[Option[Node]](None) { parent =>
                list += ItemSet(newItemSet, support)

                parent.parent
              }

              iterateParentNodes(newMaybeParent, n + 1, newItemSet)
            }

          iterateSameNodes(maybeParent, iterateParentNodes(maybeParent, n, itemSet))
        }
      }

      n = iterateSameNodes(maybeNode, n)
    } else {
      val newLocalItemSupport: Array[Int] = getLocalItemSupport(headerTableItem.maybeNode, localItemSupport)
      if (newLocalItemSupport.nonEmpty) {
        val fPTree = getLocalFPTree(headerTableItem.maybeNode, newLocalItemSupport, prefixItemSet)

        n += grow(Some(itemSet), maybeOut, localItemSupport, prefixItemSet, fPTree)
      }
    }

    n
  }

  /**
    * Mines the frequent item sets of a conditional FP-Tree by traversing the
    * header table from bottom towards top.
    *
    * @param maybeItemSet     the current item set as generated so far.
    * @param maybeOut         the provided output stream.
    * @param localItemSupport support of nodes in conditional FP-Tree.
    * @param prefixItemSet    the nodes to be added in the conditional FP-Tree.
    * @param fPTree           the conditional FP-Tree.
    */
  def grow(maybeItemSet: Option[Array[Int]],
           maybeOut: Option[PrintStream],
           localItemSupport: Array[Int],
           prefixItemSet: Array[Int],
           fPTree: FPTree): Long = {

    fPTree.headerTable.reverse.map { headerTableItem =>
      grow(headerTableItem, maybeItemSet, maybeOut, localItemSupport, prefixItemSet)
    }.sum
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
    var end = true

    @tailrec
    def iterateSameNodes(maybeNode: Option[Node], newLocalItemSupport: Array[Int]): Array[Int] =
      if (maybeNode.isEmpty) {
        newLocalItemSupport
      } else {
        val maybeNextNode = maybeNode.fold[Option[Node]](None)(_.next)
        val maybeParent = maybeNode.fold[Option[Node]](None)(_.parent)
        val support = maybeNode.fold(0)(_.count)

        @tailrec
        def iterateParentNodes(maybeParent: Option[Node], localItemSupportAccumulator: Array[Int]): Array[Int] =
          if (maybeParent.isEmpty) {
            localItemSupportAccumulator
          } else {
            val newMaybeParent = maybeParent.fold[Option[Node]](None) { parent =>
              localItemSupportAccumulator(parent.id) += support
              end = false
              parent.parent
            }

            iterateParentNodes(newMaybeParent, localItemSupportAccumulator)
          }

        iterateSameNodes(maybeNextNode, iterateParentNodes(maybeParent, newLocalItemSupport))
      }

    val finalLocalItemSupport = iterateSameNodes(maybeNode, mutableLocalItemSupport)

    if (!end) finalLocalItemSupport else new Array[Int](0)
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
    val tempPrefixItemSet = new Array[Int](prefixItemSet.length)
    prefixItemSet.copyToArray(tempPrefixItemSet)

    @tailrec
    def iterateSameNodes(maybeNode: Option[Node], fpTree: FPTree): FPTree = {
      if (maybeNode.isEmpty) {
        fpTree
      } else {
        val maybeNextNode = maybeNode.fold[Option[Node]](None)(_.next)
        val maybeParent = maybeNode.fold[Option[Node]](None)(_.parent)
        val nodeCount = maybeNode.fold(0)(_.count)

        val index = prefixItemSet.length - 1

        @tailrec
        def iterateParentNodes(maybeParent: Option[Node], initialPrefixItemSet: Array[Int], index: Int): (Array[Int], Int) = {
          if (maybeParent.isEmpty) {
            (initialPrefixItemSet, index + 1)
          } else {
            var newIndex = index
            val newMaybeParent = maybeParent.fold[Option[Node]](None) { parent =>
              initialPrefixItemSet(index) = if (localItemSupport(parent.id) >= minSupport) {
                newIndex -= 1
                parent.id
              } else {
                initialPrefixItemSet(index)
              }
              parent.parent
            }
            iterateParentNodes(newMaybeParent, initialPrefixItemSet, newIndex)
          }
        }

        val (newPrefixItemSet, newIndex) = iterateParentNodes(maybeParent, tempPrefixItemSet, index)

        if (newIndex < newPrefixItemSet.length) {
          tree.add(newIndex, newPrefixItemSet.length, newPrefixItemSet, nodeCount)
        }

        iterateSameNodes(maybeNextNode, fpTree)
      }
    }

    iterateSameNodes(maybeNode, tree)
  }
}

object FPGrowthTask {

  def props(minSupport: Int): Props = Props(new FPGrowthTask(minSupport))
}
