package ksai.core.association

import java.io.{File, FileWriter}

import akka.actor.ActorSystem
import akka.util.Timeout
import ksai.core.association.fptree.FPGrowth
import ksai.core.association.totalsupporttree.{Node, TotalSupportTree}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ARM(
           /**
             * FPGrowth algorithm object.
             */
           fPGrowth: FPGrowth,

           /**
             * Compressed set enumeration tree.
             */
           var maybeTTree: Option[TotalSupportTree] = None) {

  /**
    * List of the association rules.
    */
  var list: ArrayBuffer[AssociationRule] = new ArrayBuffer[AssociationRule]()

  /**
    * Adds an item to the FP-Tree.
    *
    * @param itemSet an item set, which should NOT contain duplicated items.
    *                Note that it is reordered after the call.
    */
  def add(itemSet: Array[Int]): Unit = fPGrowth.add(itemSet)

  /**
    * Mines the association rules.
    *
    * @param confidence the confidence threshold for the association rules.
    * @return ArrayBuffer of the discovered Association Rules.
    */
  def learn(confidence: Double)(implicit actorSystem: ActorSystem, timeout: Timeout): Future[ArrayBuffer[AssociationRule]] = {
    fPGrowth.buildTotalSupportTree().map { tTree =>
      maybeTTree = tTree

      maybeTTree.fold(list) { totalSupportTree =>
        totalSupportTree.root.maybeChildren.fold(list) { children =>

          children.map(child => Option(child)).zipWithIndex.map { case (maybeChild, index) =>
            maybeChild.fold(0L) { child =>
              val itemSet = Array(child.id)
              learn(itemSet, index, child, confidence)
            }
          }
          list
        }
      }
    }
  }

  /**
    * Generates association rules from a T-Tree.
    *
    * @param itemSet    current generated item set so far.
    * @param size       the size of the current array level in the T-Tree.
    * @param node       the current node in the T-Tree.
    * @param confidence the confidence threshold of the association rules.
    */
  def learn(itemSet: Array[Int], size: Int, node: Node, confidence: Double): Long = {
    var n: Long = 0

    if (node.maybeChildren.isEmpty) {
      n
    } else {
      var i = 0
      if (node.maybeChildren.isDefined) {
        val children = node.maybeChildren.get

        while (i < size) {

          if (Option(children(i)).isDefined) {
            val newItemSet = children(i).id +: itemSet

            n += learn(newItemSet, children(i).support, confidence)

            n += learn(newItemSet, i, children(i), confidence)
          }
          i += 1
        }
      }

      n
    }
  }

  /**
    * Generates all association rules for a given item set.
    *
    * @param itemSet    the given frequent item set.
    * @param support    the associated support value for the item set.
    * @param confidence the confidence threshold for the association rules.
    */
  def learn(itemSet: Array[Int], support: Int, confidence: Double): Long = {
    var n: Long = 0

    val combinations = getPowerSet(itemSet)

    combinations.foreach { combination =>
      val complement = getComplement(combination, itemSet)

      if (complement.nonEmpty) {
        val arc = getConfidence(combination, support)

        if (arc >= confidence) {
          val supp = support.toDouble / fPGrowth.size

          val ar = AssociationRule(combination, complement, supp, arc)
          n += 1

          list += ar
        }
      }
    }

    n
  }

  /**
    * Returns the complement of subset.
    *
    * @param subset  the subset whose complement is to be returned.
    * @param fullSet the full item set.
    */
  def getComplement(subset: Array[Int], fullSet: Array[Int]): Array[Int] = fullSet.diff(subset)

  /**
    * Returns all possible subsets except the null and full set.
    *
    * @param itemSet the item set whose combinations are to be made of.
    */
  def getPowerSet(itemSet: Array[Int]): Array[Array[Int]] =
    (1 until itemSet.length).flatMap(itemSet.combinations).toArray

  /**
    * Returns the confidence for a rule given the antecedent item set and
    * the support of the whole item set.
    *
    * @param antecedent the antecedent(LHS) of the association rule.
    * @param support    the support of the whole item set.
    */
  def getConfidence(antecedent: Array[Int], support: Double): Double =
    maybeTTree.fold(0.0)(tTree => support / tTree.getSupport(antecedent))
}

object ARM {

  /**
    * Constructor for ARM. This is a two step construction where user will
    * first scan the database to get the frequency of single items and then call
    * [[ARM.add()]] to add item sets into the FP-Tree during the second
    * scan.
    *
    * @param frequency  the frequency of single items.
    * @param minSupport the required minimum support of item sets in terms of frequency.
    */
  def apply(frequency: Array[Int], minSupport: Int): ARM = new ARM(FPGrowth(frequency, minSupport))

  /**
    * Constructor for ARM. This is a one step construction.
    *
    * @param itemSets   the given item set, which should NOT contain duplicate items.
    * @param minSupport the required minimum support of item sets in terms of percentage.
    */
  def apply(itemSets: Array[Array[Int]], minSupport: Double): ARM = new ARM(FPGrowth(itemSets, minSupport))

  /**
    * Constructor for ARM. This is a one step construction.
    *
    * @param itemSets   the given item set, which should NOT contain duplicate items.
    * @param minSupport the required minimum support of item sets in terms of frequency.
    */
  def apply(itemSets: Array[Array[Int]], minSupport: Int): ARM = new ARM(FPGrowth(itemSets, minSupport))
}
