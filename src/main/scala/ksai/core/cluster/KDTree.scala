package ksai.core.cluster

import breeze.numerics.Bessel.i1
import ksai.util.NumericFunctions

case class KDTree(
                   root: KDNode,
                   index: List[Int]
                 ) {

  /**
    * Lowerbound is basically the collection of minimum of each feature.
    * Upperbound is basically the collection of maximum of each feature.
    * @param data
    * @return
    */
  private def getLowerAndUpperBound(data: List[(List[Double], Int)]): (List[Double], List[Double]) = {
    val lowBound = data.unzip._1.head
    val uppBound = data.unzip._1.head
    data.unzip._1.foldLeft((lowBound, uppBound)) {
      case ((lbResult, ubResult), dataList) =>
        dataList.zipWithIndex.foldLeft((lbResult, ubResult)) {
          case ((lRes, uRes), (dt, idx)) =>
            val finalLBound = if (lRes(idx) > dt) {
              lRes.patch(idx, Seq(dt), 1)
            } else lRes

            val finalUBound = if (uRes(idx) < dt) {
              uRes.patch(idx, Seq(dt), 1)
            } else uRes
            (finalLBound, finalUBound)
        }
    }
  }

  private def calculateBoundingBox(lowerBound: List[Double], upperBound: List[Double]) = {
    // Calculate bounding box stats
    ((lowerBound zip upperBound) zipWithIndex).foldLeft(
      (List[Double](), List[Double](), -1.0, 0)) {
      case ((centers, radius, mxRadius, splitIdx), ((lb, ub), idx)) =>
        val centerRes = centers :+ ((lb + ub) / 2)
        val rds = ((ub - lb) / 2)
        val finalRadius = radius :+ rds
        if (rds > mxRadius) {
          (centerRes, finalRadius, rds, idx)
        } else (centerRes, finalRadius, mxRadius, splitIdx)
    }
  }

  //Reserve logic
  /*private def compressList(data: List[List[Double]], splitIndex: Int,
                           splitCutoff: Double, i1List: List[List[Double]], i2List: List[List[Double]]): (List[List[Double]], List[List[Double]]) = {
    if(data.size < 1){
      (i1List, i2List)
    } else if(data.size == 1){
      (i1List, i2List ::: data)
    } else {
      val i1Good = data.head(splitIndex) < splitCutoff
      val i2Good = data.last(splitIndex) >= splitCutoff
      val (i1Res, i2Res) = if(!i1Good && !i2Good){
        (i1List :+ data.last, i2List :+ data.head)
      } else (i1List :+ data.head, i2List :+ data.last)
      compressList(data.drop(1).dropRight(1), splitIndex, splitCutoff, i1Res, i2Res)
    }
  }*/

  private def splitNodes(data: List[(List[Double], Int)], nodeCenters: List[Double], splitIndex: Int) = {
    val splitCutoff: Double = nodeCenters(splitIndex)
//    compressList(data, splitIndex, splitCutoff, Nil, Nil)

    data.foldLeft((List[(List[Double], Int)](), List[(List[Double], Int)]())){
      case ((res1, res2), (row, index)) => if(row(splitIndex) < splitCutoff){
        (res1, res2 :+ (row, index))
      } else {
        (res1 :+ (row, index), res2)
      }
    }

  }


  private def buildNode(data: List[List[Double]]): (KDNode, List[Int]) = {
    buildNode(data.zipWithIndex)
  }

  /**
    * Build a k-d tree from the given set of data.
    */
  private def buildNode(data: List[(List[Double], Int)]): (KDNode, List[Int]) = {
    val count = data.size
    val nodeIndex = data.head._2
    val (lowerBound, upperBound) = getLowerAndUpperBound(data)
    val (nodeCenters, nodeRadiuss, maxRadius, splitIndex) = calculateBoundingBox(lowerBound, upperBound)

    // If the max spread is 0, make this a leaf node
    if (maxRadius < 1E-10) {
      val defafultNodeSum = data.unzip._1.head
      val nodeSum = if (data.size > 1) {
        defafultNodeSum.map {
          case sum => sum * data.size
        }
      } else defafultNodeSum

      val node = KDNode(count, nodeIndex, 0.0, nodeCenters, nodeRadiuss, nodeSum)
      (node, Nil)
    } else {

      val (i1Data, i2data) = splitNodes(data, nodeCenters, splitIndex)

      val (nodeLower, _) = buildNode(i1Data)
      val (nodeUpper, _) = buildNode(i2data)
      val nodeSum = (nodeLower.sum zip nodeUpper.sum).map {
        case (nlSum, nuSum) => nlSum + nuSum
      }
      val nodeMean = nodeSum.map(_ / data.size)
      val nodeCost = getNodeCost(nodeLower, nodeMean) + getNodeCost(nodeUpper, nodeMean)
      val node = KDNode(count, nodeIndex, nodeCost, nodeCenters, nodeRadiuss, nodeSum, Some(nodeLower), Some(nodeUpper))
      (node, Nil)
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
  private def getNodeCost(node: KDNode, center: List[Double]): Double = {
    val scatter = (node.sum zip center).foldLeft(0.0) {
      case (total, (sum, center)) =>
        val cost = ((sum / node.count) - center)
        total + (cost * cost)
    }
    node.cost + (node.count * scatter)
  }

  /**
    * Given k cluster centroids, this method assigns data to nearest centroids.
    * The return value is the distortion to the centroids. The parameter sums
    * will hold the sum of data for each cluster. The parameter counts hold
    * the number of data of each cluster. If membership is
    * not null, it should be an array of size n that will be filled with the
    * index of the cluster [0 - k) that each data point is assigned to.
    */
  def clustering(centroids: List[List[Double]], sums: List[List[Double]],
                 counts: List[Int], y: List[Int]): (Double, List[List[Double]], List[Int], List[Int]) = {
    val centroidSize: Int = centroids.length

    val candidates = (0 to centroidSize - 1).toList
    val newSums = sums.zipWithIndex.map {
      case (sum, index) =>
        if (index >= 0 && index < centroidSize) {
          sum.map(_ => 0.0)
        } else {
          sum
        }
    }

    filter(root, centroids, candidates, centroidSize, newSums, counts, y)
  }

  /**
    * This determines which clusters all data that are rooted node will be
    * assigned to, and updates sums, counts and membership (if not null)
    * accordingly. Candidates maintains the set of cluster indices which
    * could possibly be the closest clusters for data in this subtree.
    */
  private def filter(node: KDNode, centroids: List[List[Double]],
                     candidates: List[Int], centroidSize: Int, sums: List[List[Double]],
                     counts: List[Int], y: List[Int]): (Double, List[List[Double]], List[Int], List[Int]) = {

    // Determine which mean the node mean is closest to
    val minDist = NumericFunctions.squaredDistance(node.center, centroids(candidates(0)))
    val closest = candidates(0)

    val (_, newClosest) = candidates.drop(1).take(centroidSize).foldLeft((minDist, closest)) {
      case ((resMinDist, resClosest), candidate) =>
        val dist = NumericFunctions.squaredDistance(node.center, centroids(candidate))
        if (dist < minDist) {
          (dist, candidate)
        } else (resMinDist, resClosest)
    }

    val res = (node.lower, node.upper) match {
      case (Some(lower), Some(upper)) =>
        val (newCandidates, newK) = candidates.take(centroidSize).foldLeft(List[Int](), 0) {
          case ((result, notPrunedIndex), candidate) =>
            if (!prune(node.center, node.radius, centroids, newClosest, candidate)) {
              (result :+ candidate, notPrunedIndex + 1)
            } else {
              (result :+ 0, notPrunedIndex)
            }
        }

        // Recurse if there's at least two
        if (newK > 1) {
          val (result1, sums1, counts1, y1) = filter(lower, centroids, newCandidates, newK, sums, counts, y)
          val (result2, sums2, counts2, y2) = filter(upper, centroids, newCandidates, newK, sums1, counts1, y1)
          Some((result1 + result2, sums2, counts2, y2))
        } else None

      case _ => None
    }

    res match {
      case Some(returnResult) => returnResult
      case _ =>
        val newClosestSums: List[Double] = (sums(newClosest) zip node.sum).map {
          case (givenSum, nodeSum) => givenSum + nodeSum
        }
        val newSums: List[List[Double]] = sums.patch(newClosest, Seq(newClosestSums), 1)
        val newCounts: List[Int] = counts.patch(closest, Seq(counts(closest) + node.count), 1)
        val newY: List[Int] = y.zipWithIndex.map{
          case (yValue, index) => if(index <= node.index && (node.index + node.count) > index) {
            newClosest
          } else yValue
        }


          index.drop(node.index).take(node.index + node.count).foldLeft(y) {
          case (resY, idx) => resY.patch(idx, Seq(closest), 1)
        }

        (getNodeCost(node, centroids(newClosest)), newSums, newCounts, newY)
    }

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
  def prune(center: List[Double], radius: List[Double], centroids: List[List[Double]],
            bestIndex: Int, testIndex: Int): Boolean = {
    if (bestIndex == testIndex) {
      false
    } else {
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

object KDTree {

  def apply(data: List[List[Double]]) = {
    val n = data.length
    val index = (0 to n - 1).toList
    val emptyNode = KDNode(0)
    val tree = new KDTree(emptyNode, index)
    val (root, treeIndex) = tree.buildNode(data)
    tree.copy(root = root, index = treeIndex)
  }
}

case class KDNode(
                   count: Int,
                   index: Int, //index in the training data/record
                   cost: Double,
                   center: List[Double],
                   radius: List[Double],
                   sum: List[Double],
                   lower: Option[KDNode] = None,
                   upper: Option[KDNode] = None
                 )

object KDNode {
  def apply(index: Int) = {
    new KDNode(0,  index, 0.0, Nil, Nil, Nil)
  }

}
