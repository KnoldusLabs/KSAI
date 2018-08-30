package ksai.core.cluster

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.routing.RoundRobinPool
import akka.util.Timeout
import ksai.multithreading.KAsyncExec._
import ksai.multithreading.{KMeansActor, PruneDetail}
import ksai.util.NumericFunctions

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class BBDKDTree(
                      root: BBDKDNode,
                      index: Array[Int]
                    ) {

  /**
    * Given k cluster centroids, this method assigns data to nearest centroids.
    * The return value is the distortion to the centroids. The parameter sums
    * will hold the sum of data for each cluster. The parameter counts hold
    * the number of data of each cluster. If membership is
    * not null, it should be an array of size n that will be filled with the
    * index of the cluster [0 - k) that each data point is assigned to.
    */
  def clustering(centroids: Array[Array[Double]], sums: Array[Array[Double]],
                 counts: Array[Int], labels: Array[Int]): (Double, Array[Array[Double]], Array[Int], Array[Int]) = {
    val centroidSize: Int = centroids.length

    val candidates = (0 to centroidSize - 1).toArray
    val sumInsideSize = sums(0).size

    val newSums = (1 to centroidSize).toArray.map {
      case _ => (1 to sumInsideSize).toArray.map(_ => 0.0)
    }

    val system = ActorSystem()
    val actorRouterRef = system.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(Props[KMeansActor]))
    val result = Await.result(clusterRecursively(root, centroids, candidates, centroidSize, newSums, counts, labels, actorRouterRef), 15 * 60 seconds)
    result
  }

  /**
    * Determines whether every point in the box is closer to centroids[bestIndex] than to
    * centroids[testIndex].
    *
    * If x is a point, c_0 = centroids[bestIndex], c = centroids[testIndex], then:
    * (x-c).(x-c) < (x-c_0).(x-c_0)
    * <=> (c-c_0).(c-c_0) < 2(x-c_0).(c-c_0)
    *
    * The right-hand side is maximized for a vertex of the box where for each
    * dimension, we choose the low or high value based on the sign of x-c_0 in
    * that dimension.
    */
  def prune(center: Array[Double], radius: Array[Double], centroids: Array[Array[Double]],
            bestIndex: Int, testIndex: Int): Future[Boolean] = {
    if (bestIndex == testIndex) {
      Future.successful(false)
    } else {
      Future {
        val d = centroids(0).length
        val best = centroids(bestIndex)
        val test = centroids(testIndex)
        val lhs = 0.0
        val rhs = 0.0

        val (rLHS, rRHS) = (best.take(d) zip test.take(d) zip center.take(d) zip radius.take(d)).foldLeft((lhs, rhs)) {
          case ((resLHS, resRHS), (((bst, tst), cntr), rad)) =>
            val diff = tst - bst
            val newLHS = resLHS + (diff * diff)
            val newRHS = if (diff > 0) {
              resRHS + ((cntr + rad - bst) * diff)
            } else {
              resRHS + ((cntr - rad - bst) * diff)
            }
            (newLHS, newRHS)
        }
        rLHS >= (2 * rRHS)
      }
    }
  }

  //Reserve logic
  /*private def compressArray(data: Array[Array[Double]], splitIndex: Int,
                           splitCutoff: Double, i1Array: Array[Array[Double]], i2Array: Array[Array[Double]]): (Array[Array[Double]], Array[Array[Double]]) = {
    if(data.size < 1){
      (i1Array, i2Array)
    } else if(data.size == 1){
      (i1Array, i2Array ::: data)
    } else {
      val i1Good = data.head(splitIndex) < splitCutoff
      val i2Good = data.last(splitIndex) >= splitCutoff
      val (i1Res, i2Res) = if(!i1Good && !i2Good){
        (i1Array :+ data.last, i2Array :+ data.head)
      } else (i1Array :+ data.head, i2Array :+ data.last)
      compressArray(data.drop(1).dropRight(1), splitIndex, splitCutoff, i1Res, i2Res)
    }
  }*/

  /**
    * Lowerbound is basically the collection of minimum of each feature.
    * Upperbound is basically the collection of maximum of each feature.
    *
    * @return
    */
  private def getLowerAndUpperBound(data: Array[Array[Double]]): (Array[Double], Array[Double]) = {
    //    val lowBound = data.head
    //    val uppBound = data.head
    var finalUpperBound = data.head
    var finalLowerBound = data.head
    var i = 0
    var j = 0
    var dataRowLength = 0
    while (i < data.length) {
      val dataRow = data(i)
      dataRowLength = dataRow.length
      while (j < dataRowLength) {
        if (data(i)(j) > dataRow(j)) {
          finalLowerBound = dataRow.patch(j, Seq(dataRow(j)), 1)
        } else {
          finalLowerBound = dataRow
        }

        if (data(i)(j) < dataRow(j)) {
          finalUpperBound = dataRow.patch(j, Seq(dataRow(j)), 1)
        } else finalUpperBound = dataRow
        j += 1
      }
      i += 1
      j = 0
    }
    //    data.foldLeft((lowBound, uppBound)) {
    //      case ((lbResult, ubResult), dataRow) =>
    //        dataRow.zipWithIndex.foldLeft((lbResult, ubResult)) {
    //          case ((lRes, uRes), (dt , idx)) =>
    //            val finalLBound = if (lRes(idx) > dt) {
    //              lRes.patch(idx, Seq(dt), 1)
    //            } else lRes
    //
    //            val finalUBound = if (uRes(idx) < dt) {
    //              uRes.patch(idx, Seq(dt), 1)
    //            } else uRes
    //            (finalLBound, finalUBound)
    //        }
    //    }
    (finalLowerBound, finalUpperBound)
  }

  /**
    * ask if the size of both lower and upper bound are of same length
    **/
  private def calculateBoundingBox(lowerBound: Array[Double], upperBound: Array[Double]): (Array[Double], Array[Double], Double, Int) = {
    var lbArray = lowerBound
    var ubArray = upperBound

    var mxRadius = -1.0
    var splitIdx = 0
    var centers = new Array[Double](0)
    var radius = new Array[Double](0)

    var j = 0
    while (j < ubArray.length && j < lbArray.length) {
      centers = centers :+ (lbArray(j) + ubArray(j)) / 2
      val rds = (ubArray(j) - lbArray(j)) / 2
      radius = radius :+ rds
      if (rds > mxRadius) {
        mxRadius = rds
        splitIdx = j
      }
      j += 1
    }

    /* // Calculate bounding box stats
     ((lowerBound zip upperBound) zipWithIndex).foldLeft(
       (Array[Double](), Array[Double](), -1.0, 0)) {
       case ((centers, radius, mxRadius, splitIdx), ((lb, ub), idx)) =>
         val centerRes = centers :+ ((lb + ub) / 2)
         val rds = ((ub - lb) / 2)
         val finalRadius = radius :+ rds
         if (rds > mxRadius) {
           (centerRes, finalRadius, rds, idx)
         } else (centerRes, finalRadius, mxRadius, splitIdx)
     }*/
    (centers, radius, mxRadius, splitIdx)
  }

  private def splitNodes(data: Array[(Array[Double], Int)], nodeCenters: Array[Double], splitIndex: Int) = {
    val splitCutoff: Double = nodeCenters(splitIndex)
    //    compressArray(data, splitIndex, splitCutoff, Array(), Array())

    //
    //    data.foldLeft((Array[(Array[Double], Int)](), Array[(Array[Double], Int)]())){
    //      case ((res1, res2), (row, index)) => if(row(splitIndex) < splitCutoff){
    //        (res1, res2 :+ (row, index))
    //      } else {
    //        (res1 :+ (row, index), res2)
    //      }
    //    }
    var res1 = Array[(Array[Double], Int)]()
    var res2 = Array[(Array[Double], Int)]()
    var dataIndex = 0

    while (dataIndex < data.length) {
      val (row, index) = data(dataIndex)
      if (row(splitIndex) < splitCutoff) res2 = res2 :+ (row, index)
      else res1 = res1 :+ (row, index)
      dataIndex += 1
    }
    (res1, res2)
  }

  private def buildNode(data: Array[Array[Double]]): (BBDKDNode, Array[Int]) = {
    buildNode(data.zipWithIndex)
  }

  /**
    * Build a k-d tree from the given set of data.
    */
  private def buildNode(zipData: Array[(Array[Double], Int)]): (BBDKDNode, Array[Int]) = {
    val count = zipData.size
    val (data, indices) = zipData.unzip
    val nodeIndex = indices.head
    val (lowerBound, upperBound) = getLowerAndUpperBound(data)

    val (nodeCenters, nodeRadius, maxRadius, splitIndex) = calculateBoundingBox(lowerBound, upperBound)

    // If the max spread is 0, make this a leaf node
    if (maxRadius < 1E-10) {
      val defaultNodeSum = data.head
      var nodeSumArray = new Array[Double](0)
      /*val nodeSum = */if (data.length > 1) {
        var i = 0
        while(i < defaultNodeSum.length){
          nodeSumArray = nodeSumArray :+ defaultNodeSum(i) * data.length
          i += 1
        }
       /* defafultNodeSum.map {
          case sum => sum * data.size
        }*/
      } else nodeSumArray = defaultNodeSum

      val node = BBDKDNode(count, nodeIndex, 0.0, nodeCenters, nodeRadius, nodeSumArray)
      (node, Array())
    } else {

      val (i1Data, i2data) = splitNodes(zipData, nodeCenters, splitIndex)

      val (nodeLower, _) = buildNode(i1Data)
      val (nodeUpper, _) = buildNode(i2data)
      var nodeSumArray = new Array[Double](0)
      var i = 0
      while(i < nodeLower.sum.length && i < nodeUpper.sum.length){
        nodeSumArray =  nodeSumArray :+  nodeLower.sum(i) + nodeUpper.sum(i)
        i += 1
      }
      /*val nodeSum = (nodeLower.sum zip nodeUpper.sum).map {
        case (nlSum, nuSum) => nlSum + nuSum
      }*/
      val nodeMean = nodeSumArray.map(_ / zipData.size)
      val nodeCost = getNodeCost(nodeLower, nodeMean) + getNodeCost(nodeUpper, nodeMean)
      val node = BBDKDNode(count, nodeIndex, nodeCost, nodeCenters, nodeRadius, nodeSumArray, Some(nodeLower), Some(nodeUpper))
      (node, Array())
    }
  }

  /**
    * Returns the total contribution of all data in the given kd-tree node,
    * assuming they are all assigned to a mean at the given location.
    *
    * sum_{x \in node} ||x - mean||^2.
    *
    * If c denotes the mean of mass of the data in this node and n denotes
    * the number of data in it, then this quantity is given by
    *
    * n * ||c - mean||^2 + sum_{x \in node} ||x - c||^2
    *
    * The sum is precomputed for each node as cost. This formula follows
    * from expanding both sides as dot products.
    */
  private def getNodeCost(node: BBDKDNode, center: Array[Double]): Double = {
    var total = 0.0
    var i = 0

    while(i < node.sum.length && i < center.length){
      val cost = (node.sum(i) / node.count) - center(i)
      total += cost * cost
      i += 1
    }
    val scatter = total

    /*val scatter = (node.sum zip center).foldLeft(0.0) {
      case (total, (sum, center)) =>
        val cost = ((sum / node.count) - center)
        total + (cost * cost)
    }*/
    node.cost + (node.count * scatter)
  }

  private def findClosestCentroidCandidate(node: BBDKDNode, candidates: Array[Int],
                                           centroidSize: Int, centroids: Array[Array[Double]]) = {
    // Determine which mean the node mean is closest to
    /*val minDist = NumericFunctions.squaredDistance(node.center, centroids(candidates(0)))
    val closest = candidates(0)*/
    var minDist = NumericFunctions.squaredDistance(node.center, centroids(candidates(0)))
    var closest = candidates(0)

    val array = candidates.drop(1).take(centroidSize)
    var i = 0

    while(i < array.length){
      val dist = NumericFunctions.squaredDistance(node.center, centroids(array(i)))
      if(dist < minDist){
        minDist = dist
        closest = array(i)
      }
      i += 1
    }
    (minDist, closest)
    /*candidates.drop(1).take(centroidSize).foldLeft((minDist, closest)) {
      case ((resMinDist, resClosest), candidate) =>
        val dist = NumericFunctions.squaredDistance(node.center, centroids(candidate))
        if (dist < minDist) {
          (dist, candidate)
        } else (resMinDist, resClosest)
    }*/
  }

  private def pruneAsync(node: BBDKDNode, centroids: Array[Array[Double]],
                         candidates: Array[Int], centroidSize: Int, closestCentroidCandidate: Int, pruneActorRef: ActorRef) = {

    implicit val timeout = Timeout(20 seconds)
    val pruneCandidates: Array[Future[(Int, Int)]] = candidates.take(centroidSize).map {
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
    val prunedFutureArray = Future.sequence(pruneCandidates.toList)
    prunedFutureArray.map {
      prunedCandidates =>
        val (newCandidates, notPrunedIndexCount) = prunedCandidates.toArray.unzip
        (newCandidates, notPrunedIndexCount.sum)
    }
  }

  /**
    * This determines which clusters all data that are rooted node will be
    * assigned to, and updates sums, counts and membership (if not null)
    * accordingly. Candidates maintains the set of cluster indices which
    * could possibly be the closest clusters for data in this subtree.
    *
    * @param node The node which
    * @param centroids
    * @param candidates
    * @param centroidSize
    * @param sums
    * @param labels
    * @return
    */
  private def clusterRecursively(node: BBDKDNode, centroids: Array[Array[Double]],
                                 candidates: Array[Int], centroidSize: Int, sums: Array[Array[Double]],
                                 counts: Array[Int], labels: Array[Int], pruneActorRef: ActorRef): Future[(Double, Array[Array[Double]], Array[Int], Array[Int])] = {

    val (_, closestCentroidCandidate) = findClosestCentroidCandidate(node, candidates, centroidSize, centroids)
    val res = (node.lower, node.upper) match {
      case (Some(lower), Some(upper)) =>

      pruneAsync(node, centroids, candidates, centroidSize, closestCentroidCandidate, pruneActorRef).flatMap{
          case (newCandidates, notPrunedIndexCount) =>
            if (notPrunedIndexCount > 1) {
             for {
                (cost1, sums1, counts1, labels1) <- clusterRecursively(lower, centroids, newCandidates, notPrunedIndexCount, sums, counts, labels, pruneActorRef)
                (cost2, sums2, counts2, labels2) <- clusterRecursively(upper, centroids, newCandidates, notPrunedIndexCount, sums1, counts1, labels1, pruneActorRef)
              } yield Some((cost1 + cost2, sums2, counts2, labels2))
            } else Future.successful(None)
        }
      case _ => Future.successful(None)
    }

    res.map {
      case Some(returnResult) => returnResult
      case _ =>
        val newClosestSums: Array[Double] = (sums(closestCentroidCandidate) zip node.sum).map {
          case (closestSum, nodeSum) => closestSum + nodeSum
        }
        val newSums: Array[Array[Double]] = sums.patch(closestCentroidCandidate, Seq(newClosestSums), 1)
        val newCounts: Array[Int] = counts.patch(closestCentroidCandidate, Seq(counts(closestCentroidCandidate) + node.count), 1)
        val newLabels: Array[Int] = labels.zipWithIndex.map {
          case (yValue, index) => if (index <= node.index && (node.index + node.count) > index) {
            closestCentroidCandidate
          } else yValue
        }

        (getNodeCost(node, centroids(closestCentroidCandidate)), newSums, newCounts, newLabels)
    }

  }

}

object BBDKDTree {

  def apply(data: Array[Array[Double]]) = {
    val n = data.length
    val index = (0 to n - 1).toArray
    val emptyNode = BBDKDNode(0)
    val tree = new BBDKDTree(emptyNode, index)
    val (root, treeIndex) = tree.buildNode(data)
    tree.copy(root = root, index = treeIndex)
  }
}

case class BBDKDNode(
                      count: Int,
                      index: Int, //index in the training data/record
                      cost: Double,
                      center: Array[Double],
                      radius: Array[Double],
                      sum: Array[Double],
                      lower: Option[BBDKDNode] = None,
                      upper: Option[BBDKDNode] = None
                    )

object BBDKDNode {
  def apply(index: Int) = {
    new BBDKDNode(0, index, 0.0, Array(), Array(), Array())
  }

}
