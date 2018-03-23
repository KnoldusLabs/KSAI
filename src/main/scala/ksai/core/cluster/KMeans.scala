package ksai.core.cluster

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.routing.RoundRobinPool
import akka.util.Timeout
import ksai.multithreading.KAsyncExec._
import ksai.multithreading.{FindCentroidDistance, KMeansActor}
import ksai.util.NumericFunctions

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random
import scala.util.control.NonFatal
import ksai.logging.ScalaLogging.logger._

import scala.collection.mutable.ListBuffer

case class KMeans(
                   k: Int,
                   y: List[Int],
                   size: List[Int],
                   distortion: Double,
                   centroids: List[List[Double]]
                 ) {

  def predict(x: List[Double]): Int = {
    val (best, _) = (0 to k - 1).foldLeft((0, Double.MaxValue)) {
      case ((bestCluster, minDist), idx) =>
        val dist = NumericFunctions.squaredDistance(x, centroids(idx))
        if (dist < minDist) {
          (idx, dist)
        } else (bestCluster, minDist)
    }
    best
  }
}


object KMeans {

  private def init(kdTree: BBDKDTree, data: ListBuffer[ListBuffer[Double]], k: Int, maxIter: Int)(implicit system: ActorSystem): KMeans = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k)
    }
    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter)
    }
    val d = data.head.length
    val distortion = Double.MaxValue
    val y = seed(data, k, EUCLIDEAN)
    info(s"Labels count after seeding = ${y.size}")
    val size: List[Int] = (0 to k - 1).toList.map(_ => 0)
    val kdInitials: List[List[Double]] = (0 to k - 1).toList.map(kv => (0 to d - 1).toList.map(_ => 0.0))
    val ygroup = y.groupBy(value => value)
    val newSize: List[Int] = size.zipWithIndex.map {
      case (_, indx) => ygroup.get(indx).map(_.size).fold(0)(identity)
    }

    val newCentroids = (0 to y.size - 1).map {
      index => (0 to d-1).map {
        innerIndex => kdInitials(y(index))(innerIndex) + data(index)(innerIndex)
      }
    }

    val sizeDivideCentroids = (newSize.zipWithIndex).map {
      case (sz, idx) => newCentroids(idx).map(centd => centd / sz).toList
    }

    val (dist, newSums, firstClusteredSize, labels1) = kdTree.clustering(sizeDivideCentroids, kdInitials, newSize, y)
    info(s"Initial cluster distortion $dist")
    val (finalDistortion, _, _, finalLabels, finalCounts, finalCentroids, _) = (1 to maxIter - 1).toList.foldLeft(
      (distortion, dist, newSums, labels1, firstClusteredSize, sizeDivideCentroids, false)) {
      case ((resDistortion, resDist, resSums, resLabels, resCounts, resCentroids, isMinimumDistornFound), idx) =>

        if (!isMinimumDistornFound) {

          val (dist1, newSums1, secondClusteredSize, newLabels) = kdTree.clustering(resCentroids, resSums, resCounts, resLabels)
          info(s"Iteration count = $idx")
          val sumReplacedCentroids = ((resCentroids zip resSums) zip secondClusteredSize).map {
            case ((sdc, sms), s) =>
              if (s > 0) {
                sms.map(sm => sm / s)
              } else sdc
          }

          if (resDistortion <= dist1) {
            (resDistortion, resDist, resSums, resLabels, firstClusteredSize, resCentroids, true)
          } else {
            (dist1, dist1, newSums1, newLabels, secondClusteredSize, sumReplacedCentroids, isMinimumDistornFound)
          }
        } else (resDistortion, resDist, resSums, resLabels, firstClusteredSize, resCentroids, isMinimumDistornFound)


    }
    new KMeans(k = k, y = finalLabels, size = finalCounts, distortion = finalDistortion, centroids = finalCentroids)
  }

  /**
    * Clustering data into k clusters. Run the algorithm for given times
    * and return the best one with smallest distortion.
    *
    * @param data    the input data of which each row is a sample.
    * @param k       the number of clusters.
    * @param maxIter the maximum number of iterations for each running.
    * @param runs    the number of runs of K-Means algorithm.
    */
  def apply(data: List[List[Double]], k: Int, maxIter: Int, runs: Int, bbdTree: Option[BBDKDTree] = None)(implicit system: ActorSystem): Future[KMeans] = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k)
    }

    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter)
    }

    if (runs <= 0) {
      throw new IllegalArgumentException("Invalid number of runs: " + runs)
    }

    info(s"Before kd tree got initialized where bbdkTree is ${bbdTree.isDefined} and the data size is ${data.size}")

    val bbd = bbdTree.getOrElse(BBDKDTree(data))

    info(s"BBDTree root nodes count: ${bbd.root.count}")

    val bufferedData = data.map(_.to[ListBuffer]).to[ListBuffer]
    val defaultKMeans = init(bbd, bufferedData, k, maxIter)
    info(s"Got the initial distortion ${defaultKMeans.distortion}")
    val futKMeans = (1 to runs - 1).toList.map {
      case _ => init(bbd, bufferedData, k, maxIter)
    }
    try{
    val result = futKMeans.foldLeft(defaultKMeans) {/*.map {*/
//      case allkmeans =>
//        allkmeans.foldLeft(defaultKMeans) {
          case (result, nextKMeans) =>
            if (nextKMeans.distortion < result.distortion) {
              nextKMeans
            } else result
        }
      Future.successful(result)
    } catch {
      case NonFatal(ex) =>
        ex.printStackTrace()
        val actorRouterRef = system.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(Props[KMeansActor]))
        (0 to runs - 1).foldLeft(Future.successful(defaultKMeans)) {
          case (bestFut, _) =>
            bestFut.flatMap {
              case best =>
                lloyd(data, k, maxIter, actorRouterRef).map {
                  case nextKMeans =>
                    if (nextKMeans.distortion < best.distortion) {
                      nextKMeans
                    } else best
                }
            }
        }
    }

  }

  private def findDistortionsAndLabels(data: ListBuffer[ListBuffer[Double]], distortions: ListBuffer[Double], y: ListBuffer[Int],
                                       distanceModel: ClusteringDistance, centroid: ListBuffer[Double], kCount: Int) = {
    ((data zip distortions) zip y).map {
      case ((dataRow, distortion), yValue) =>
        val dist = distanceModel match {
          case EUCLIDEAN => NumericFunctions.squaredDistance(dataRow, centroid)
          case EUCLIDEAN_MISSING_VALUES => NumericFunctions.squaredDistanceWithMissingValues(dataRow, centroid)
          case JENSEN_SHANNON_DIVERGENCE => NumericFunctions.jensenShannonDivergence(dataRow, centroid)
        }

        if (dist < distortion) {
          (dist, kCount - 1)
        } else (distortion, yValue)
    }.unzip
  }

  private def findCentroid(distortions: ListBuffer[Double], cutOff: Double) = {
    distortions.foldLeft((0.0, -1)) {
      case ((cost, index), distortion) =>
        val costSum = cost + distortion
        if (costSum >= cutOff) {
          (cost, index)
        } else (cost, index + 1)
    }
  }

  def seed(data: ListBuffer[ListBuffer[Double]], k: Int, distanceModel: ClusteringDistance) = {
    val n = data.length
    val centroid: ListBuffer[Double] = data(Random.self.nextInt(n))
    val distortions = (0 to n - 1).toList.map(_ => Double.MaxValue).to[ListBuffer]
    val y = (0 to n - 1).toList.map(_ => 0).to[ListBuffer]

    val (newDistortions, ys, newCentroids) = (1 to k - 1).toList.foldLeft((distortions, y, new ListBuffer[Double]())) {
      case ((foldedDistortions, foldedLabels, resCentroid), j) =>
        val (newDistortions, labels) = findDistortionsAndLabels(data, foldedDistortions, foldedLabels, distanceModel, centroid, j)
        val cutoff: Double = Math.random() * newDistortions.sum
        val (_, centroidIndex) = findCentroid(newDistortions, cutoff)

        (newDistortions, labels, data(centroidIndex))
    }

    val (_, finalY) = findDistortionsAndLabels(data, newDistortions, ys, distanceModel, newCentroids, k)

    finalY.toList
  }

  def lloyd(data: List[List[Double]], k: Int, maxIter: Int, runs: Int): Future[KMeans] = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k)
    }

    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter)
    }

    if (runs <= 0) {
      throw new IllegalArgumentException("Invalid number of runs: " + runs)
    }
    val system = ActorSystem()
    val actorRouterRef = system.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(Props[KMeansActor]))

    for {
      defaultKMeans <- lloyd(data, k, maxIter, actorRouterRef)
      finalKMeans <- (0 to runs - 1).foldLeft(Future.successful(defaultKMeans)) {
        case (bestFut, _) =>
          bestFut.flatMap {
            case best =>
              lloyd(data, k, maxIter, actorRouterRef).map {
                case nextKMeans =>
                  if (nextKMeans.distortion < best.distortion) {
                    nextKMeans
                  } else best
              }
          }
      }
    } yield finalKMeans
  }

  def apply(data: List[List[Double]], k: Int, maxIter: Int): Future[KMeans] = {
    val system = ActorSystem()
    val actorRouterRef = system.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(Props[KMeansActor]))

    lloyd(data, k, maxIter, actorRouterRef)
  }

  def lloyd(data: List[List[Double]], k: Int, maxIter: Int, kmeansActorRef: ActorRef): Future[KMeans] = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k)
    }
    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter)
    }
    val initialDistortion = Double.MaxValue
    val bufferedData = data.map(_.to[ListBuffer]).to[ListBuffer]
    val y = seed(bufferedData, k, EUCLIDEAN)
    (0 to maxIter - 1).foldLeft(Future.successful((y, initialDistortion, true))) {
      case (defaultResult, itr) =>
        defaultResult.flatMap {
          case (prevYs, distortion, isMore) =>
            if (isMore) {
              val (newCentroids, _) = calculateCentroidsAndSize(k, prevYs, data)
              asyncSquareDistance(data, newCentroids, prevYs, kmeansActorRef).map {
                case (ys, wcss) =>
                  println(s"Iteration $itr")
                  if (distortion <= wcss) {
                    (ys, distortion, false)
                  } else {
                    (ys, wcss, isMore)
                  }
              }
            } else Future.successful((prevYs, distortion, isMore))
        }
    }.map {
      case (resY, distortion, _) =>
        val (finalCentroids, finalSize) = calculateCentroidsAndSize(k, resY, data)
        new KMeans(k, resY, finalSize, distortion, finalCentroids)
    }

  }

  private def asyncSquareDistance(data: List[List[Double]], centroids: List[List[Double]],
                                  y: List[Int], kmeansActorRef: ActorRef): Future[(List[Int], Double)] = {
    implicit val timeout = Timeout(20 seconds)
    val dataPieces = data.zipWithIndex
    val centroidIndexAndDistance: List[Future[(Int, Int, Double)]] = dataPieces.map {
      case (dt, index) => (kmeansActorRef ? FindCentroidDistance(centroids, index, dt)).map {
        case (yIndex: Int, nearest: Double) => (index, yIndex, nearest)
      }
    }

    Future.sequence(centroidIndexAndDistance).map {
      case distanceAndYs =>
        distanceAndYs.foldLeft((y, 0.0)) {
          case ((ys, wcss), (dataIndex: Int, yIndex: Int, nearest: Double)) =>
            val newYs = if (yIndex != -1) {
              ys.patch(dataIndex, Seq(yIndex), 1)
            } else ys
            (newYs, wcss + nearest)
        }
    }
  }

  private def calculateCentroidsAndSize(k: Int, y: List[Int], data: List[List[Double]]) = {
    val d = data(0).length
    val initialSize: List[Int] = (0 to k - 1).toList.map(_ => 0)
    val initialCentroids = (0 to k - 1).toList.map(_ => (0 to d - 1).toList.map(_ => 0.0))
    val reinitializedSize = initialSize.zipWithIndex.map { case (size, idx) => size + y.filter(_ == idx).size }

    val yCentIndices = y.groupBy(yValue => yValue)

    val reinitializedCentroids = initialCentroids.zipWithIndex.map {
      case (initCent, indx) =>
        yCentIndices.get(indx) match {
          case Some(numSameLabelData) =>
            numSameLabelData.foldLeft(initCent) {
              case (resultList, yIndex) => (data(yIndex) zip resultList).map { case (dt, dt2) => dt + dt2 }
            } match {
              case Nil => initCent
              case dataSum => dataSum.map(_ / numSameLabelData.size)
            }

          case None => initCent
        }
    }
    (reinitializedCentroids, reinitializedSize)
  }

}

trait ClusteringDistance

case object EUCLIDEAN extends ClusteringDistance

case object EUCLIDEAN_MISSING_VALUES extends ClusteringDistance

case object JENSEN_SHANNON_DIVERGENCE extends ClusteringDistance