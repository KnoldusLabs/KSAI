package ksai.core.association

import ksai.core.association.fptree.FPGrowth
import ksai.core.association.totalsupporttree.{Node, TotalSupportTree}

import scala.collection.mutable.ArrayBuffer

class ARM(
           /**
             * FPGrowth algorithm object.
             */
           fPGrowth: FPGrowth,

           /**
             * Compressed set enumeration tree.
             */
           var maybeTtree: Option[TotalSupportTree] = None) {

  /**
    * List of the association rules.
    */
  var list: ArrayBuffer[AssociationRule] = new ArrayBuffer[AssociationRule]()

  /**
    * Adds an item to the FP-Tree.
    *
    * @param itemset an itemset, which should NOT contain duplicated items.
    *                Note that it is reordered after the call.
    */
  def add(itemset: Array[Int]): Unit = fPGrowth.add(itemset)

  /**
    * Mines the association rules.
    *
    * @param confidence the confidence threshold for the association rules.
    * @return ArrayBuffer of the discovered Association Rules.
    */
  def learn(confidence: Double): ArrayBuffer[AssociationRule] = {
    maybeTtree = fPGrowth.buildTotalSupportTree()

    if (maybeTtree.isDefined) {
      val ttree = maybeTtree.get

      if (ttree.root.maybeChildren.isDefined) {
        val children = ttree.root.maybeChildren.get

        var i = 0
        while (i < children.length) {
          if (Option(children(i)).isDefined) {
            val itemset = Array(children(i).id)
            learn(itemset, i, children(i), confidence)
          }

          i += 1
        }
      }
    }

    list
  }

  /**
    * Generates association rules from a T-Tree.
    *
    * @param itemset    current generated itemset so far.
    * @param size       the size of the current array level in the T-Tree.
    * @param node       the current node in the T-Tree.
    * @param confidence the confidence threshold of the association rules.
    */
  def learn(itemset: Array[Int], size: Int, node: Node, confidence: Double): Long = {
    var n: Long = 0

    if (node.maybeChildren.isEmpty) {
      n
    } else {
      var i = 0
      if (node.maybeChildren.isDefined) {
        val children = node.maybeChildren.get

        while (i < size) {

          if (Option(children(i)).isDefined) {
            val newItemset = children(i).id +: itemset

            n += learn(newItemset, children(i).support, confidence)

            n += learn(newItemset, i, children(i), confidence)
          }
          i += 1
        }
      }

      n
    }
  }

  /**
    * Generates all association rules for a given itemset.
    *
    * @param itemset    the given frequent itemset.
    * @param support    the associated support value for the itemset.
    * @param confidence the confidence threshold for the association rules.
    */
  def learn(itemset: Array[Int], support: Int, confidence: Double): Long = {
    var n: Long = 0

    val combinations = getPowerSet(itemset)

    combinations.foreach { combination =>
      val complement = getComplement(combination, itemset)

      if (complement.nonEmpty) {
        val arc = getConfidence(combination, support)

        if (arc >= confidence) {
          val supp = support.toDouble / fPGrowth.size

          val ar = AssociationRule(combination, complement, supp, arc)
          //println(ar)
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
    * @param fullset the full itemset.
    */
  def getComplement(subset: Array[Int], fullset: Array[Int]): Array[Int] = fullset.diff(subset)

  /**
    * Returns all possible subsets except the null and full set.
    *
    * @param itemset the itemset whose combinations are to be made of.
    */
  def getPowerSet(itemset: Array[Int]): Array[Array[Int]] =
    (1 until itemset.length).flatMap(itemset.combinations).toArray

  /**
    * Returns the confidence for a rule given the antecedent itemset and
    * the support of the whole itemset.
    *
    * @param antecedent the antecedent(LHS) of the association rule.
    * @param support    the support of the whole itemset.
    */
  def getConfidence(antecedent: Array[Int], support: Double): Double =
    maybeTtree.fold(0.0)(ttree => support / ttree.getSupport(antecedent))
}

object ARM {

  /**
    * Constructor for ARM. This is a two step construction where user will
    * first scan the database to get the frequency of single items and then call
    * [[ARM.add()]] to add itemsets into the FP-Tree during the second
    * scan.
    *
    * @param frequency  the frequency of single items.
    * @param minSupport the required minimum support of itemsets in terms of frequency.
    */
  def apply(frequency: Array[Int], minSupport: Int): ARM = new ARM(FPGrowth(frequency, minSupport))

  /**
    * Constructor for ARM. This is a one step construction.
    *
    * @param itemsets   the given itemset, which should NOT contain duplicate items.
    * @param minSupport the required minimum support of itemsets in terms of percentage.
    */
  def apply(itemsets: Array[Array[Int]], minSupport: Double): ARM = new ARM(FPGrowth(itemsets, minSupport))

  /**
    * Constructor for ARM. This is a one step construction.
    *
    * @param itemsets   the given itemset, which should NOT contain duplicate items.
    * @param minSupport the required minimum support of itemsets in terms of frequency.
    */
  def apply(itemsets: Array[Array[Int]], minSupport: Int): ARM = new ARM(FPGrowth(itemsets, minSupport))
}
