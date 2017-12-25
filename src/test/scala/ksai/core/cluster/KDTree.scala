package ksai.core.cluster

import ksai.util.NumericFunctions

case class KDTree(
                   root: KDNode,
                   index: List[Int]
                 ) {
  /**
    * Build a k-d tree from the given set of data.
    */
  private def buildNode(data: List[List[Double]], begin: Int, end: Int): (KDNode, List[Int]) = {
    val count = end - begin
    val lowBound = data(index(begin))
    val uppBound = data(index(begin))

    val (newLowBound, newUppBound) = data.drop(index(begin)).foldLeft((lowBound, uppBound)) {
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

    // Calculate bounding box stats
    val (nodeCenter, nodeRadius, maxRadius, splitIndex) = ((newLowBound zip newUppBound) zipWithIndex).foldLeft(
      (List[Double](), List[Double](), -1.0, -1)) {
      case ((centers, radiuss, mxRadius, splitIdx), ((lb, ub), idx)) =>
        val centerRes = centers :+ ((lb + ub) / 2)
        val rds = ((ub - lb) / 2)
        val radiusRes = radiuss :+ rds
        if (rds > mxRadius) {
          (centerRes, radiusRes, rds, idx)
        } else (centerRes, radiusRes, mxRadius, splitIdx)
    }

    // If the max spread is 0, make this a leaf node
    if (maxRadius < 1E-10) {
      val defafultNodeSum = data(index(begin))

      val nodeSum = if (end > begin + 1) {
        val len: Double = end - begin;
        defafultNodeSum.map {
          case sum => sum * len
        }
      } else defafultNodeSum
      val node = KDNode(count, begin, 0.0, nodeCenter, nodeRadius, nodeSum)
      (node, index)
    } else {
      // Partition the data around the midpoint in this dimension. The
      // partitioning is done in-place by iterating from left-to-right and
      // right-to-left in the same way that partioning is done in quicksort.
      val splitCutoff = nodeCenter(splitIndex)
      val reverseIndex = end - 1

      val (nodeIndex, _, _, nodeSize) = data.drop(index(begin)).foldLeft((index, begin, reverseIndex, 0)) {
        case ((indexList, indexPoint, reverseIndex, size), dataList) =>
          val i1Good = dataList(splitIndex) < splitCutoff
          val i2Good = dataList(splitIndex) >= splitCutoff

          val (resSwappedIndex, newI1Good, newI2Good) = if (!i1Good && !i2Good) {
            val swappedIndex = indexList.patch(indexPoint, Seq(indexList(reverseIndex)), 1)
            val swappedIndexI2 = swappedIndex.patch(reverseIndex, Seq(indexList(indexPoint)), 1)
            (swappedIndexI2, true, true)
          } else (indexList, i1Good, i2Good)

          val (resIndexPoint, resSize) = if (newI1Good) {
            val newIndexPoint = indexPoint + 1
            val newSize = size + 1
            (newIndexPoint, newSize)
          } else (indexPoint, size)

          val resReverseIndex = if (newI2Good) {
            reverseIndex + 1
          } else reverseIndex

          (resSwappedIndex, resIndexPoint, resReverseIndex, resSize)
      }

      // Create the child nodes
      val (nodeLower, _) = buildNode(data, begin, begin + nodeSize)
      val (nodeUpper, _) = buildNode(data, begin + nodeSize, end)
      val nodeSum = (nodeLower.sum zip nodeUpper.sum).map {
        case (nlSum, nuSum) => nlSum + nuSum
      }
      val nodeMean = nodeSum.map(_ / count)
      val nodeCost = getNodeCost(nodeLower, nodeMean) + getNodeCost(nodeUpper, nodeMean)
      val node = KDNode(count, begin, nodeCost, nodeCenter, nodeRadius, nodeSum, Some(nodeLower), Some(nodeUpper))
      (node, nodeIndex)
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
    * This determines which clusters all data that are rooted node will be
    * assigned to, and updates sums, counts and membership (if not null)
    * accordingly. Candidates maintains the set of cluster indices which
    * could possibly be the closest clusters for data in this subtree.
    */
  private def filter(node: KDNode, centroids: List[List[Double]], candidates: List[Int], k: Int,
                     sums: List[List[Double]], counts: List[Int], membership: List[Int]): Double = {

    val d = centroids(0).length

    // Determine which mean the node mean is closest to
    val minDist = NumericFunctions.squaredDistance(node.center, centroids(candidates(0)))
    val closest = candidates(0)

    val (newMinDist, newClosest) = candidates.drop(1).take(k).foldLeft((minDist, closest)) {
      case ((resMinDist, resClosest), candidate) =>
        val dist = NumericFunctions.squaredDistance(node.center, centroids(candidate))
        if (dist < minDist) {
          (dist, candidate)
        } else (resMinDist, resClosest)
    }

    (node.lower, node.upper) match {
//      case None =>
      case (Some(lower), Some(upper)) =>
    val (newCandidates, newK) = candidates.take(k).foldLeft(List[Int](), 0){
      case ((result, notPrunedIndex), candidate) =>
        if (!prune(node.center, node.radius, centroids, newClosest, candidate)) {
          (result :+ candidate, notPrunedIndex + 1)
        } else {
          (result :+ 0, notPrunedIndex)
        }
    }

      // Recurse if there's at least two
    if (newK > 1) {
    val result = filter(lower, centroids, newCandidates, newK, sums, counts, membership) +
      filter(upper, centroids, newCandidates, newK, sums, counts, membership)
    result
    }

    }

    // Assigns all data within this node to a single mean
    for (int i = 0; i < d; i ++){
      sums[closest][i] += node.sum[i];
    }

    counts[closest] += node.count;

    if (membership != null) {
      int last = node.index + node.count;
      for (int i = node.index; i < last; i ++ ) {
        membership[index[i]] = closest;
      }
    }

    return getNodeCost(node, centroids[closest]);
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
    val (root, treeIndex) = tree.buildNode(data, 0, n)
    tree.copy(root = root, index = treeIndex)
  }
}

case class KDNode(
                   count: Int,
                   index: Int,
                   cost: Double,
                   center: List[Double],
                   radius: List[Double],
                   sum: List[Double],
                   lower: Option[KDNode] = None,
                   upper: Option[KDNode] = None
                 )

object KDNode {
  def apply(index: Int) = {
    new KDNode(0, index, 0.0, Nil, Nil, Nil)
  }

}
