package ksai.core.association.fptree

import java.io.PrintStream

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.routing.RoundRobinPool
import akka.util.Timeout
import ksai.core.association.totalsupporttree.TotalSupportTree

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps

class FPGrowth(
                /**
                  * The required minimum support of the item sets.
                  */
                minSupport: Int,

                /**
                  * The FP-Tree
                  */
                val T0: FPTree) {
  private val actorSystem = ActorSystem("FP-Growth")

  private val fPGrowthTask =
    actorSystem.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2)
      .props(FPGrowthTask.props(minSupport)))

  implicit val timeout: Timeout = Timeout(45 seconds)

  /**
    * The list of frequent item sets.
    */
  var list: ArrayBuffer[ItemSet] = new ArrayBuffer[ItemSet]()
  /**
    * The total support tree.
    */
  var maybeTTree: Option[TotalSupportTree] = None

  /**
    * Add an item set into the FP-Tree.
    *
    * @param itemSet an item set, that should NOT contain duplicated items.
    *                Note that it is reordered after the call.
    */
  def add(itemSet: Array[Int]): Unit = T0.add(itemSet)

  /**
    * Returns the number of transactions in the FP-Tree.
    */
  def size: Int = T0.size

  /**
    * Mines the frequent item sets. Discovered frequent item sets are stored in a
    * total support tree.
    */
  def buildTotalSupportTree(): Future[Option[TotalSupportTree]] = {
    maybeTTree = Some(TotalSupportTree(T0.order, minSupport, T0.numFreqItems))
    learn(None).map(_ => maybeTTree)
  }

  /**
    * Mines the frequent item sets. These item sets are put in the [[list]]
    * and returned.
    */
  def learn(): Future[ArrayBuffer[ItemSet]] = learn(None).map(_ => list)

  /**
    * Mines the frequent item sets. These item sets are printed out
    * to the provided stream.
    *
    * @param maybeOut the provided stream.
    */
  def learn(maybeOut: Option[PrintStream]): Future[Long] = grow(maybeOut, T0, None)

  /**
    * Mines the frequent item sets. Start with the bottom of the header table and
    * work upwards. For each available FP-Tree Node -
    * <OL>
    * <LI> Count the support.</LI>
    * <LI> Build up item set so far.</LI>
    * <LI> Add to supported sets.</LI>
    * <LI> Build a new FP-Tree: (i) Create a new local root, (ii) Create a new
    * header table, and (iii) populate with ancestors.</LI>
    * <LI> If new local FP-Tree is not empty, repeat the mining operation.</LI>
    * </OL>
    * Otherwise end.
    *
    * @param maybeOut     the provided output stream.
    * @param fPTree       the FP-Tree to mine from.
    * @param maybeItemSet the current item set as generated so far.
    */
  def grow(maybeOut: Option[PrintStream], fPTree: FPTree, maybeItemSet: Option[Array[Int]]): Future[Long] = {
    Future.sequence(fPTree.headerTable.reverse.toList.map { headerTableItem =>

      val localItemSet = new Array[Int](T0.numItems)
      val prefixItemSet = new Array[Int](T0.maxItemSetSize)

      (fPGrowthTask ? Grow(headerTableItem, maybeItemSet, maybeOut, localItemSet, prefixItemSet)).mapTo[Rules]
    }).map { allRules =>
      val totalRules = allRules.foldLeft(Rules(0, list)) { (accumulator, currentRules) =>
        Rules(accumulator.nrOfRules + currentRules.nrOfRules, accumulator.rules ++ currentRules.rules)
      }
      list = totalRules.rules

      list.foreach { itemSet =>
        maybeTTree = maybeTTree.fold[Option[TotalSupportTree]](None) { tTree =>
          tTree.add(itemSet.items, itemSet.support)
          Some(tTree)
        }
      }

      totalRules.nrOfRules
    }
  }
}

object FPGrowth {

  def apply(magnet: FPGrowthMagnet): magnet.Result = magnet()
}
