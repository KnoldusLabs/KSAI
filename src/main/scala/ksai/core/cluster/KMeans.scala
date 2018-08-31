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

case class KMeans(
                   k: Int,
                   y: Array[Int],
                   size: Array[Int],
                   distortion: Double,
                   centroids: Array[Array[Double]]
                 ) {

  /**
    * Cluster a new instance.
    *
    * @param x a new instance.
    * @return the cluster label, which is the index of nearest centroid.
    */
  def predict(x: Array[Double]): Int = {
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

  private def init(kdTree: BBDKDTree, data: Array[Array[Double]], k: Int, maxIter: Int): KMeans = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k)
    }
    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter)
    }
    val d = data.head.length
    val distortion = Double.MaxValue
    val y = seed(data, k, EUCLIDEAN)
    println(s"............got seed ${y.size}")
    val size: Array[Int] = (0 to k - 1).toArray.map(_ => 0)
    val kdInitials: Array[Array[Double]] = (0 to k - 1).toArray.map(kv => (0 to d - 1).toArray.map(_ => 0.0))
    val ygroup = y.groupBy(value => value)
    val newSize: Array[Int] = size.zipWithIndex.map {
      case (_, indx) => ygroup.get(indx).map(_.size).fold(0)(identity)
    }

    val rowSumForCentroids = (y zip data).map {
      case (yValue: Int, dataRow: Array[Double]) =>
        dataRow.zipWithIndex.map {
          case (dt, idx) => kdInitials(yValue)(idx) + dt
        }
    }

    val meanCentroids = (newSize.zipWithIndex).map {
      case (sz, idx) => rowSumForCentroids(idx).map(centd => centd / sz)
    }

    val (dist, newSums, firstClusteredSize, labels1) = kdTree.clustering(meanCentroids, kdInitials, newSize, y)
    println("........Done with first clustering")
    val (finalDistortion, _, _, finalLabels, finalCounts, finalCentroids, _) = (1 to maxIter - 1).toArray.foldLeft(
      (distortion, dist, newSums, labels1, firstClusteredSize, meanCentroids, false)) {
      case ((resDistortion, resDist, resSums, resLabels, resCounts, resCentroids, isMinimumDistornFound), idx) =>

        if (!isMinimumDistornFound) {

          val (dist1, newSums1, secondClusteredSize, newLabels) = kdTree.clustering(resCentroids, resSums, resCounts, resLabels)
          println(s"..........................$idx")
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
  def apply(data: Array[Array[Double]], k: Int, maxIter: Int, runs: Int): Future[KMeans] = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k)
    }

    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter)
    }

    if (runs <= 0) {
      throw new IllegalArgumentException("Invalid number of runs: " + runs)
    }

    println(".........before kdtree")

    val bbd = BBDKDTree(data)

    println(s"...............${bbd.root.count}")

    val defaultKMeans = init(bbd, data, k, maxIter)
    println(s"...............Got the best distortion ${defaultKMeans.distortion}")
    val futKMeans: Array[Future[KMeans]] = (0 to runs - 1).toArray.map {
      case _ => Future(init(bbd, data, k, maxIter))
    }
    Future.sequence(futKMeans.toList).map {
      case allkmeans =>
        allkmeans.toArray.foldLeft(defaultKMeans) {
          case (result, nextKMeans) =>
            if (nextKMeans.distortion < result.distortion) {
              nextKMeans
            } else result
        }

    }.recoverWith {
      case NonFatal(ex) =>
        ex.printStackTrace()
        val system = ActorSystem()
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

  private def findDistortionsAndLabels(data: Array[Array[Double]], distortions: Array[Double], y: Array[Int],
                                       distanceModel: ClusteringDistance, centroid: Array[Double], kCount: Int) = {
    /*((data zip distortions) zip y).map {
      case ((dataRow, distortion), yValue) =>
        val dist = distanceModel match {
          case EUCLIDEAN => NumericFunctions.squaredDistance(dataRow, centroid)
          case EUCLIDEAN_MISSING_VALUES => NumericFunctions.squaredDistanceWithMissingValues(dataRow, centroid)
          case JENSEN_SHANNON_DIVERGENCE => NumericFunctions.jensenShannonDivergence(dataRow, centroid)
        }

        if (dist < distortion) {
          (dist, kCount - 1)
        } else (distortion, yValue)
    }.unzip*/

    val distortionsToReturn: Array[Double] = new Array[Double](distortions.length)
    val labelsToReturn: Array[Int] = new Array[Int](y.length)
    var i = 0
    while (i < data.length && i < distortions.length && i < y.length) {
      val dist = distanceModel match {
        case EUCLIDEAN => NumericFunctions.squaredDistance(data(i), centroid)
        case EUCLIDEAN_MISSING_VALUES => NumericFunctions.squaredDistanceWithMissingValues(data(i), centroid)
        case JENSEN_SHANNON_DIVERGENCE => NumericFunctions.jensenShannonDivergence(data(i), centroid)
      }

      val (distortionToReturn, labelToReturn) = if (dist < distortions(i)) {
        (dist, kCount - 1)
      } else (distortions(i), y(i))

      distortionsToReturn(i) = distortionToReturn
      labelsToReturn(i) = labelToReturn

      i += 1
    }

    (distortionsToReturn, labelsToReturn)
  }

  private def findCentroid(distortions: Array[Double], cutOff: Double) = {
    distortions.foldLeft((0.0, -1)) {
      case ((cost, index), distortion) =>
        val costSum = cost + distortion
        if (costSum >= cutOff) {
          (cost, index)
        } else (cost, index + 1)
    }
  }

  def seed(data: Array[Array[Double]], k: Int, distanceModel: ClusteringDistance): Array[Int] = {
    val n = data.length
    val centroid: Array[Double] = data(Random.self.nextInt(n))
    val distortions: Array[Double] = new Array[Double](n)
    val y: Array[Int] = new Array[Int](n)

    var i = 0
    while (i < n) {
      distortions(i) = Double.MaxValue
      y(i) = 0
      i += 1
    }

    val (newDistortions, ys, newCentroids) = (1 until k).foldLeft((distortions, y, Array[Double]())) {
      case ((foldedDistortions, foldedLabels, resCentroid), j) =>
        val (newDistortions, labels) = findDistortionsAndLabels(data, foldedDistortions, foldedLabels, distanceModel, centroid, j)
        val cutoff: Double = Math.random() * newDistortions.sum
        val (_, centroidIndex) = findCentroid(newDistortions, cutoff)

        (newDistortions, labels, data(centroidIndex))
    }

    val (_, finalY) = findDistortionsAndLabels(data, newDistortions, ys, distanceModel, newCentroids, k)

    finalY
  }

  def lloyd(data: Array[Array[Double]], k: Int, maxIter: Int, runs: Int): Future[KMeans] = {
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

  def apply(data: Array[Array[Double]], k: Int, maxIter: Int)(implicit actorSystem: ActorSystem): Future[KMeans] = {
    val actorRouterRef = actorSystem.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(Props[KMeansActor]))

    lloyd(data, k, maxIter, actorRouterRef)
  }

  def lloyd(data: Array[Array[Double]], k: Int, maxIter: Int, kmeansActorRef: ActorRef): Future[KMeans] = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k)
    }
    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter)
    }
    val initialDistortion = Double.MaxValue
    val y = seed(data, k, EUCLIDEAN)
    (0 until maxIter).foldLeft(Future.successful((y, initialDistortion, true))) {
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

  private def asyncSquareDistance(data: Array[Array[Double]], centroids: Array[Array[Double]],
                                  y: Array[Int], kmeansActorRef: ActorRef): Future[(Array[Int], Double)] = {
    implicit val timeout = Timeout(20 seconds)
    val dataPieces = data.zipWithIndex
    val centroidIndexAndDistance: Array[Future[(Int, Int, Double)]] = dataPieces.map {
      case (dataRow, index) => (kmeansActorRef ? FindCentroidDistance(centroids, index, dataRow)).map {
        case (yIndex: Int, nearest: Double) => (index, yIndex, nearest)
      }
    }

    Future.sequence(centroidIndexAndDistance.toList).map {
      case distanceAndYs =>
        distanceAndYs.toArray.foldLeft((y, 0.0)) {
          case ((ys, wcss), (dataIndex: Int, yIndex: Int, nearest: Double)) =>
            val newYs = if (yIndex != -1) {
              ys.patch(dataIndex, Seq(yIndex), 1)
            } else ys
            (newYs, wcss + nearest)
        }
    }
  }

  private def calculateCentroidsAndSize(k: Int, y: Array[Int], data: Array[Array[Double]]) = {
    val d = data(0).length
    val initialSize: Array[Int] = Array.fill(k)(0)
    val initialCentroids = Array.fill(k)(Array.fill(d)(0.0))
    val reinitializedSize = new Array[Int](initialSize.length)
    initialSize.indices.foreach { index => reinitializedSize(index) = initialSize(index) + y.count(_ == index) }

    val yCentIndices = y.groupBy(yValue => yValue)

    /*val reinitializedCentroids = initialCentroids.zipWithIndex.map {
      case (initCent, indx) =>
        yCentIndices.get(indx) match {
        case Some(numSameLabelData) =>
          numSameLabelData.foldLeft(initCent) {
            case (resultArray, yIndex) => (data(yIndex) zip resultArray).map { case (dt, dt2) => dt + dt2 }
          } match {
            case Nil => initCent
            case dataSum => dataSum.map(_ / numSameLabelData.size)
          }

        case None => initCent
        }
    }*/

    val reinitializedCentroids = Array.ofDim[Double](initialCentroids.length, initialCentroids(0).length)
    var indx = 0
    while (indx < initialCentroids.length) {
        reinitializedCentroids(indx) = yCentIndices.get(indx) match {
        case Some(numSameLabelData) =>
          val n = numSameLabelData.length
          val dataSum = initialCentroids(indx)
          var i = 0
          var j = 0
          while (i < n) {
            while (j < dataSum.length) {
              dataSum(j) += data(numSameLabelData(j))(j)
              j += 1
            }
            i += 1
          }

          if (dataSum.isEmpty) initialCentroids(indx) else dataSum.map(_ / numSameLabelData.length)

        case None => initialCentroids(indx)
      }

      indx += 1
    }

    (reinitializedCentroids, reinitializedSize)
  }

}

trait ClusteringDistance

case object EUCLIDEAN extends ClusteringDistance

case object EUCLIDEAN_MISSING_VALUES extends ClusteringDistance

case object JENSEN_SHANNON_DIVERGENCE extends ClusteringDistance