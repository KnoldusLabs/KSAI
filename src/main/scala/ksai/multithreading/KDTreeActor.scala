package ksai.multithreading

import akka.actor.{Actor, ActorLogging, Props, Stash}
import akka.pattern.{ask, pipe}
import akka.routing.RoundRobinPool
import akka.util.Timeout
import ksai.core.cluster.BBDKDNode
import ksai.multithreading.KAsyncExec._
import ksai.util.NumericFunctions

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._


class KDTreeActor extends Actor with ActorLogging {

  override def receive: Receive = {
    case InternalFilter(node: BBDKDNode, centroids: ListBuffer[ListBuffer[Double]],
    candidates: ListBuffer[Int], centroidSize: Int, sums: ListBuffer[ListBuffer[Double]],
    counts: ListBuffer[Int], labels: ListBuffer[Int]) =>
      val closestCentroidCandidate = findClosestCentroidCandidate(node, candidates, centroidSize, centroids)
      val pruneResult = (node.lower, node.upper) match {
        case (Some(lower), Some(upper)) =>
          val (newCandidates, notPrunedIndexCount) = pruneIteration(node, centroids, candidates, centroidSize, closestCentroidCandidate)
          if (notPrunedIndexCount > 1) {
            FilterResult(0.0, sums, counts, labels, notPrunedIndexCount, closestCentroidCandidate, node.index, node.count, newCandidates)
          } else {
            rearrangeAllNodeProperties(node, sums, counts, labels, centroids, closestCentroidCandidate, newCandidates)
          }
        case _ => rearrangeAllNodeProperties(node, sums, counts, labels, centroids, closestCentroidCandidate, candidates)
      }

      sender() ! pruneResult

  }


  private def pruneIteration(node: BBDKDNode, centroids: ListBuffer[ListBuffer[Double]],
                         candidates: ListBuffer[Int], centroidSize: Int, closestCentroidCandidate: Int) = {

    val newCandidates = new ListBuffer[Int]()
    var notPrunedIndexCount = 0
    (0 to centroidSize-1).foreach { index =>
        val isPruned: Boolean = prune(node.center, node.radius, centroids, closestCentroidCandidate, candidates(index))
        if (!isPruned) {
          newCandidates += candidates(index)
          notPrunedIndexCount = notPrunedIndexCount + 1
        } else {
          newCandidates += 0
        }
    }
    (newCandidates, notPrunedIndexCount)
  }

  private def prune(center: ListBuffer[Double], radius: ListBuffer[Double], centroids: ListBuffer[ListBuffer[Double]],
                    bestIndex: Int, testIndex: Int) = {
    if (bestIndex == testIndex) {
      false
    } else {
      val d = centroids(0).length
      val best = centroids(bestIndex)
      val test = centroids(testIndex)

      val (rLHS, rRHS) = (0 to d - 1).foldLeft((0.0, 0.0)) {
        case ((resLHS, resRHS), index) =>
          val diff = test(index) - best(index)
          val newLHS = resLHS + (diff * diff)
          val newRHS = if (diff > 0) {
            resRHS + ((center(index) + radius(index) - best(index)) * diff)
          } else {
            resRHS + ((center(index) - radius(index) - best(index)) * diff)
          }
          (newLHS, newRHS)
      }
      rLHS >= (2 * rRHS)
    }
  }

  private def findClosestCentroidCandidate(node: BBDKDNode, candidates: ListBuffer[Int],
                                           centroidSize: Int, centroids: ListBuffer[ListBuffer[Double]]) = {
    // Determine which mean the node mean is closest to
    var minDist = NumericFunctions.squaredDistance(node.center, centroids(candidates(0)))
    var closest: Int = candidates.head

    (1 to centroidSize -1).foreach{ index =>
      val dist = NumericFunctions.squaredDistance(node.center, centroids(candidates(index)))
      if (dist < minDist) {
        closest = candidates(index)
        minDist = dist
      }
    }
   closest
  }

  private def rearrangeAllNodeProperties(node: BBDKDNode, sums: ListBuffer[ListBuffer[Double]], counts: ListBuffer[Int],
                                         labels: ListBuffer[Int], centroids: ListBuffer[ListBuffer[Double]],
                                         closestCentroidCandidate: Int, candidates: ListBuffer[Int]) = {
    val closestSums = sums(closestCentroidCandidate)
    val newClosestSums: ListBuffer[Double] = new ListBuffer[Double]()
    (0 to closestSums.size - 1).foreach {
      case index => newClosestSums += closestSums(index) + node.sum(index)
    }
    sums(closestCentroidCandidate) = newClosestSums
    counts(closestCentroidCandidate) = counts(closestCentroidCandidate) + node.count
    (node.index to node.index + node.count - 1).foreach(index => labels(index) = closestCentroidCandidate)

    FilterResult(getNodeCost(node, centroids(closestCentroidCandidate)), sums, counts, labels, 0,
      closestCentroidCandidate, node.index, node.count, candidates)
  }

  private def getNodeCost(node: BBDKDNode, center: ListBuffer[Double]): Double = {
    val scatter = (0 to node.sum.size - 1).foldLeft(0.0) {
      case (total, index) =>
        val cost = ((node.sum(index) / node.count) - center(index))
        total + (cost * cost)
    }
    node.cost + (node.count * scatter)
  }

}

class KDFilterActor extends Actor with Stash with ActorLogging {

  implicit val timeout = Timeout(60 * 60 seconds)
  val kdRouter = context.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(Props[KDTreeActor]))
  val kdMerger = context.actorOf(Props[KDMergerActor])
  var resultsCount = 0
  var callCount = 0
  var parent = context.system.deadLetters
  var sums = new ListBuffer[ListBuffer[Double]]()

  override def receive: Receive = start

  def start: Receive = {
    case fc@FilterCluster(node: BBDKDNode, centroids: List[List[Double]], candidates: List[Int],
    centroidSize: Int, fcSums: List[List[Double]], fcCounts: List[Int], fcLabels: List[Int]) => {
      log.info("Got message for clustering")
      parent = sender()
      kdMerger ! fc
      callCount = 0
      context.become(recurse)
      self ! InternalFilter(node, centroids.map(_.to[ListBuffer]).to[ListBuffer], candidates.to[ListBuffer],
        centroidSize, fcSums.map(_.to[ListBuffer]).to[ListBuffer], fcCounts.to[ListBuffer], fcLabels.to[ListBuffer])
    }
    case _ => stash()
  }

  def recurse: Receive = {
    case internalFilter:InternalFilter =>
      val kdResult = (kdRouter ? internalFilter)
      kdResult.map {
        case result: FilterResult =>
          (internalFilter, result)
      } pipeTo self

    case (internalFilter: InternalFilter, result: FilterResult) => {
      kdMerger ! result
      resultsCount = resultsCount + 1
      if (result.notPrunedIndexCount > 1) {
        (internalFilter.node.lower, internalFilter.node.upper) match {
          case (Some(lower), Some(upper)) =>
            self ! KeepTheCount(2)
            self ! internalFilter.copy(node = lower, candidates = result.candidates)
            self ! internalFilter.copy(node = upper, candidates = result.candidates)
          case _ => self ! KeepTheCount(0)
        }
      } else {
        self ! KeepTheCount(0)
      }
    }

    case KeepTheCount(count) => {
      callCount = callCount + count
//      log.info(s"Keepcount Result count is ${resultsCount} and the message call count is ${callCount}")
      if (count == 0 && callCount + 1 == resultsCount) {
        self ! "return"
      }
    }

    case "return" => {
      log.info(s"Result count is ${resultsCount} and the message call count is ${callCount}")
      (kdMerger ? "return") pipeTo parent
      //      parent ! (cost, sums, counts, labels)
      unstashAll()
      context.unbecome()
    }
  }

}

class KDMergerActor extends Actor with ActorLogging {

  var cost = 0.0
  var sums = new ListBuffer[ListBuffer[Double]]()
  var counts = new ListBuffer[Int]()
  var labels = new ListBuffer[Int]()

  override def receive: Receive = {
    case fc@FilterCluster(node: BBDKDNode, centroids: List[List[Double]], candidates: List[Int],
    centroidSize: Int, fcSums: List[List[Double]], fcCounts: List[Int], fcLabels: List[Int]) => {
      labels = fcLabels.to[ListBuffer]
      counts = fcCounts.to[ListBuffer]
      sums = fcSums.map(_.to[ListBuffer]).to[ListBuffer]
    }

    case result: FilterResult =>
      //      log.info(s"Result count is ${resultsCount} and the message call count is ${callCount}")
      if (sums.isEmpty) {
        sums = result.sums
      } else {
        (0 to sums.size - 1).foreach {
          case index =>
            if ((sums(index)).filterNot(pr => pr == 0.0).size == 0) {
              sums(index) = result.sums(index)
            } else if (result.sums(index).filterNot(pr => pr == 0.0).size == 0) {
              sums(index) = sums(index)
            } else {
              sums(index) = result.sums(index)
            }
        }
      }

      (result.nodeIndex to result.nodeIndex + result.nodeCount - 1).foreach(index => labels(index) = result.closestCandidate)
      counts(result.closestCandidate) = counts(result.closestCandidate) + result.nodeCount
      cost = cost + result.cost
    case "return" => sender() !(cost, sums, counts, labels)
  }
}

case class FilterCluster(
                          node: BBDKDNode,
                          centroids: List[List[Double]],
                          candidates: List[Int],
                          centroidSize: Int,
                          sums: List[List[Double]],
                          counts: List[Int],
                          labels: List[Int]
                        )

case class FilterResult(
                         cost: Double,
                         sums: ListBuffer[ListBuffer[Double]],
                         counts: ListBuffer[Int],
                         labels: ListBuffer[Int],
                         notPrunedIndexCount: Int,
                         closestCandidate: Int,
                         nodeIndex: Int,
                         nodeCount: Int,
                         candidates: ListBuffer[Int]
                       )

case class InternalFilter(node: BBDKDNode,
                          centroids: ListBuffer[ListBuffer[Double]],
                          candidates: ListBuffer[Int],
                          centroidSize: Int,
                          sums: ListBuffer[ListBuffer[Double]],
                          counts: ListBuffer[Int],
                          labels: ListBuffer[Int])

case class KeepTheCount(count: Int)
