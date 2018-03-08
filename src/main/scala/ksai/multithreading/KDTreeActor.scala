package ksai.multithreading

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.pattern.ask
import akka.routing.RoundRobinPool
import akka.util.Timeout
import ksai.core.cluster.BBDKDNode
import ksai.multithreading.KAsyncExec._
import ksai.util.NumericFunctions

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.duration._


class KDTreeActor extends Actor {

  val pruneActorRef = context.actorOf(Props[KMeansActor])

  var parent: ActorRef = context.system.deadLetters

  override def receive: Receive = {
    case FilterCluster(node: BBDKDNode, centroids: List[List[Double]],
    candidates: List[Int], centroidSize: Int, sums: List[List[Double]],
    counts: List[Int], labels: List[Int]) =>
      parent = sender()
      //      println("Received message in KDTreeActor")
      val (_, closestCentroidCandidate: Int) = findClosestCentroidCandidate(node, candidates, centroidSize, centroids)
      //      println(s"Cosest centroid candidate ${closestCentroidCandidate}")
      (node.lower, node.upper) match {
        case (Some(lower), Some(upper)) =>
          pruneAsync(node, centroids, candidates, centroidSize, closestCentroidCandidate).map {
            case (newCandidates, notPrunedIndexCount) =>
              //            println(s"New Candidates ${newCandidates}")

              if (notPrunedIndexCount > 1) {
                parent ! FilterResult(0.0, sums, counts, labels, notPrunedIndexCount, closestCentroidCandidate, node.index, node.count)
              } else {
                parent ! rearrangeAllNodeProperties(node, sums, counts, labels, centroids, closestCentroidCandidate)
              }
          }
        case _ => parent ! rearrangeAllNodeProperties(node, sums, counts, labels, centroids, closestCentroidCandidate)
      }

  }


  private def pruneAsync(node: BBDKDNode, centroids: List[List[Double]],
                         candidates: List[Int], centroidSize: Int, closestCentroidCandidate: Int) = {

    implicit val timeout = Timeout(20 seconds)
    val pruneCandidates: List[Future[(Int, Int)]] = candidates.take(centroidSize).map {
      case candidate =>
        val pruneResult: Future[Any] = pruneActorRef ? PruneDetail(node.center, node.radius, centroids, closestCentroidCandidate, candidate)
        pruneResult.map {
          case isPruned: Boolean =>
            if (!isPruned) {
              (candidate, 1)
            } else {
              (0, 0)
            }
        }
    }
    val prunedFutureList = Future.sequence(pruneCandidates)
    prunedFutureList.map {
      prunedCandidates =>
        val (newCandidates, notPrunedIndexCount) = prunedCandidates.unzip
        (newCandidates, notPrunedIndexCount.sum)
    }
  }

  private def findClosestCentroidCandidate(node: BBDKDNode, candidates: List[Int],
                                           centroidSize: Int, centroids: List[List[Double]]) = {
    // Determine which mean the node mean is closest to
    val minDist = NumericFunctions.squaredDistance(node.center, centroids(candidates(0)))
    val closest = candidates.head

    candidates.drop(1).take(centroidSize).foldLeft((minDist, closest)) {
      case ((resMinDist, resClosest), candidate) =>
        val dist = NumericFunctions.squaredDistance(node.center, centroids(candidate))
        if (dist < minDist) {
          (dist, candidate)
        } else (resMinDist, resClosest)
    }
  }

  private def rearrangeAllNodeProperties(node: BBDKDNode, sums: List[List[Double]], counts: List[Int],
                                         labels: List[Int], centroids: List[List[Double]], closestCentroidCandidate: Int) = {
    val newClosestSums: List[Double] = (sums(closestCentroidCandidate) zip node.sum).map {
      case (closestSum, nodeSum) => closestSum + nodeSum
    }
    val newSums: List[List[Double]] = sums.patch(closestCentroidCandidate, Seq(newClosestSums), 1)
    val newCounts: List[Int] = counts.patch(closestCentroidCandidate, Seq(counts(closestCentroidCandidate) + node.count), 1)
    val newLabels: List[Int] = labels.zipWithIndex.map {
      case (yValue, index) => if (index <= node.index && (node.index + node.count) > index) {
        closestCentroidCandidate
      } else yValue
    }

    FilterResult(getNodeCost(node, centroids(closestCentroidCandidate)), newSums, newCounts, newLabels, 0, closestCentroidCandidate, node.index, node.count)
  }

  private def getNodeCost(node: BBDKDNode, center: List[Double]): Double = {
    val scatter = (node.sum zip center).foldLeft(0.0) {
      case (total, (sum, center)) =>
        val cost = ((sum / node.count) - center)
        total + (cost * cost)
    }
    node.cost + (node.count * scatter)
  }

}

class KDFilterActor extends Actor with Stash {

  implicit val timeout = Timeout(60 * 60 seconds)
  val kdRouter = context.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(Props[KDTreeActor]))
  var results = new ListBuffer[FilterResult]()
  var callCount = 0
  var parent = context.system.deadLetters

  override def receive: Receive = start

  def start: Receive = {
    case fc@FilterCluster(node: BBDKDNode, centroids: List[List[Double]], candidates: List[Int],
    centroidSize: Int, sums: List[List[Double]], counts: List[Int], labels: List[Int]) => {
      parent = sender()
      callCount = 0
      results = new ListBuffer[FilterResult]()
      context.become(recurse)
      self ! InternalFilter(fc)
    }
    case _ => stash()
  }

  def recurse: Receive = {
    case InternalFilter(fc@FilterCluster(node: BBDKDNode, centroids: List[List[Double]], candidates: List[Int],
    centroidSize: Int, sums: List[List[Double]], counts: List[Int], labels: List[Int])) =>
      //      println(s"InternalFilter ..............${fc.node.index}")
      val kdResult = (kdRouter ? fc)
      kdResult.map {
        case result: FilterResult =>
          self ! (fc, result)
      }
    case (fc: FilterCluster, result: FilterResult) => {
      results += result
      //          println(s"Added new result to the list now its size is ${results.size}")

      if (result.notPrunedIndexCount > 1) {
        (fc.node.lower, fc.node.upper) match {
          case (Some(lower), Some(upper)) =>
            self ! KeepTheCount(2)
            self ! InternalFilter(fc.copy(node = lower))
            self ! InternalFilter(fc.copy(node = upper))
          case _ => self ! KeepTheCount(0)
        }
      } else {
        self ! KeepTheCount(0)
      }
    }
    case KeepTheCount(count) => {
      callCount = callCount + count
      println(s"Now the call count is $callCount and the results size is ${results.size}")
      if (callCount == results.size) {
        self ! "return"
      }

    }
    case "return" => {
      val filteredValues = results.toList.foldLeft((0.0, List[List[Double]](), List[Int](), List[Int]())) {
        case ((cost, sums, counts, labels), result) =>
          val resSum = sums match {
            case Nil => result.sums
            case prevSums => (prevSums zip result.sums).map {
              case (prevRow, resRow) =>
                if (prevRow.filterNot(pr => pr == 0.0).size == 0) {
                  resRow
                } else if (resRow.filterNot(pr => pr == 0.0).size == 0) {
                  prevRow
                } else {
                  resRow
                }
            }
          }
          val newLabels = labels.zipWithIndex.map {
            case (yValue, index) => if (index <= result.nodeIndex && (result.nodeCount + result.nodeCount) > index) {
              result.closestCandidate
            } else yValue
          }
          val newCounts = counts.patch(result.closestCandidate, Seq(counts(result.closestCandidate) + result.nodeCount), 1)

          (cost + result.cost, resSum, newCounts, newLabels)
      }
      parent ! filteredValues
      unstashAll()
      context.unbecome()
    }
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
                         sums: List[List[Double]],
                         counts: List[Int],
                         labels: List[Int],
                         notPrunedIndexCount: Int,
                         closestCandidate: Int,
                         nodeIndex: Int,
                         nodeCount: Int
                       )

case class InternalFilter(fc: FilterCluster)

case class KeepTheCount(count: Int)
