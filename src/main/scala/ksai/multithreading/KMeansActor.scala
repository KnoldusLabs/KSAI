package ksai.multithreading

import akka.actor.Actor
import ksai.util.NumericFunctions

class KMeansActor extends Actor {
  override def receive: Receive = {
    case PruneDetail(center, radius, centroids, bestIndex, testIndex) =>
      val pruneResult = if (bestIndex == testIndex) {
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
      sender() ! pruneResult

    case FindCentroidDistance(centroids: Array[Array[Double]], dataIndex: Int, dt: Array[Double]) =>
      val result = findDistanceFromCentroid(centroids, dataIndex, dt)
      sender() ! result
  }

  private def findDistanceFromCentroid(centroids: Array[Array[Double]], dataIndex: Int, dt: Array[Double]): (Int, Double) = {
    var i = 0
    var nearest = Double.MaxValue
    var yIndx = -1
    while (i < centroids.length) {
      val squaredDistance = NumericFunctions.squaredDistance(dt, centroids(i))
      if (nearest > squaredDistance) {
        yIndx = i
        nearest = squaredDistance
      }

      i += 1
    }

    (yIndx, nearest)
  }

}

case class PruneDetail(
                        center: Array[Double],
                        radius: Array[Double],
                        centroids: Array[Array[Double]],
                        bestIndex: Int,
                        testIndex: Int
                      )

case class FindCentroidDistance(
                                 centroids: Array[Array[Double]],
                                 dataIndex: Int,
                                 dt: Array[Double]
                               )
