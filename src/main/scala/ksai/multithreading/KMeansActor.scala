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

        val (rLHS, rRHS) = (0 to d-1).foldLeft((0.0, 0.0)){
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
      sender() ! pruneResult

    case FindCentroidDistance(centroids: List[List[Double]], dataIndex: Int, dt: List[Double]) =>
      val result = findDistanceFromCentroid(centroids, dataIndex, dt)
      sender() ! result
  }

  private def findDistanceFromCentroid(centroids: List[List[Double]], dataIndex: Int, dt: List[Double]) = {
    centroids.zipWithIndex.foldLeft((-1, Double.MaxValue)) {
      case ((yIndex, nearest), (cents, centIndx)) =>
        val squredDistance = NumericFunctions.squaredDistance(dt, cents)
        if (nearest > squredDistance) {
          (centIndx, squredDistance)
        } else {
          (yIndex, nearest)
        }
    }
  }

}

case class PruneDetail(
                        center: List[Double],
                        radius: List[Double],
                        centroids: List[List[Double]],
                        bestIndex: Int,
                        testIndex: Int
                      )

case class FindCentroidDistance(
                                 centroids: List[List[Double]],
                                 dataIndex: Int,
                                 dt: List[Double]
                               )
