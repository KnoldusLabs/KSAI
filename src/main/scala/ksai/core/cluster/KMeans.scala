package ksai.core.cluster

import ksai.util.NumericFunctions
import spire.std.double

import scala.util.Random
import ksai.util.DoubleUtil._

case class KMeans(
                   k: Int,
                   y: List[Int],
                   size: List[Int],
                   distortion: Double,
                   centroids: List[List[Double]]
                 ) {


}


object KMeans {

  def apply(bbd: KDTree, data: List[List[Double]], k: Int, maxIter: Int) = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k);
    }
    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter);
    }
    val d = data(0).length
    val distortion = Double.MaxValue
    val y = seed(data, k, EUCLIDEAN)
    val size: List[Int] = (0 to k - 1).toList.map(_ => 0)
    val kdInitials: List[List[Double]] = (0 to k - 1).toList.map(kv => (0 to d - 1).toList.map(_ => 0.0))
    val newSize = y.map(yv => size(yv) + 1)

    val newCentroids = (y zip data).map {
      case (yValue: Int, dataList: List[Double]) =>
        dataList.zipWithIndex.map {
          case (dt, idx) => kdInitials(yValue)(idx) + dt
        }
    }

    val sizeDivideCentroids = (newSize.zipWithIndex).map {
      case (s, idx) => newCentroids(idx).map(cen => cen / s)
    }

    val (dist, newSums, newCounts, newMembership) = bbd.clustering(sizeDivideCentroids, kdInitials, newSize, y)
    val (finalDistortion, _, _, finalCounts, finalMembership, finalCentroids) = (1 to maxIter - 1).toList.foldLeft(
      (distortion, dist, newSums, newCounts, newMembership, sizeDivideCentroids)) {
      case ((resDistortion, resDist, resSums, resCounts, resMembership, resCentroids), _) =>
        val (dist1, newSums1, newCounts1, newMembership1) = bbd.clustering(resCentroids, resSums, resCounts, resMembership)
        val sumReplacedCentroids = ((resCentroids zip resSums) zip size).map {
          case ((sdc, sms), s) =>
            if (s > 0) {
              sms.map(sm => sm / s)
            } else sdc
        }

        if (resDistortion <= dist1) {
          (resDistortion, resDist, resSums, resCounts, resMembership, resCentroids)
        } else {

          (dist1, dist1, newSums1, newCounts1, newMembership1, sumReplacedCentroids)
        }
    }
    new KMeans(k = k, y = finalMembership, size = finalCounts, distortion = finalDistortion, centroids = finalCentroids)
  }


  /**
    * Initialize cluster membership of input objects with KMeans++ algorithm.
    * Many clustering methods, e.g. k-means, need a initial clustering
    * configuration as a seed.
    * <p>
    * K-Means++ is based on the intuition of spreading the k initial cluster
    * centers away from each other. The first cluster center is chosen uniformly
    * at random from the data points that are being clustered, after which each
    * subsequent cluster center is chosen from the remaining data points with
    * probability proportional to its distance squared to the point's closest
    * cluster center.
    * <p>
    * The exact algorithm is as follows:
    * <ol>
    * <li> Choose one center uniformly at random from among the data points. </li>
    * <li> For each data point x, compute D(x), the distance between x and the nearest center that has already been chosen. </li>
    * <li> Choose one new data point at random as a new center, using a weighted probability distribution where a point x is chosen with probability proportional to D<sup>2</sup>(x). </li>
    * <li> Repeat Steps 2 and 3 until k centers have been chosen. </li>
    * <li> Now that the initial centers have been chosen, proceed using standard k-means clustering. </li>
    * </ol>
    * This seeding method gives out considerable improvements in the final error
    * of k-means. Although the initial selection in the algorithm takes extra time,
    * the k-means part itself converges very fast after this seeding and thus
    * the algorithm actually lowers the computation time too.
    *
    * <h2>References</h2>
    * <ol>
    * <li> D. Arthur and S. Vassilvitskii. "K-means++: the advantages of careful seeding". ACM-SIAM symposium on Discrete algorithms, 1027-1035, 2007.</li>
    * <li> Anna D. Peterson, Arka P. Ghosh and Ranjan Maitra. A systematic evaluation of different methods for initializing the K-means clustering algorithm. 2010.</li>
    * </ol>
    *
    * @param data data objects to be clustered.
    * @param k    the number of cluster.
    * @return the cluster labels.
    */
  def seed(data: List[List[Double]], k: Int, distance: ClusteringDistance): List[Int] = {
    val n = data.length
    val centroid: List[Double] = data(Random.self.nextInt(n))
    val d: List[Double] = (0 to n - 1).toList.map(_ => Double.MaxValue)
    val y: List[Int] = (0 to n - 1).toList.map(_ => 0)

    val (ds, ys, centroids) = (1 to k - 1).toList.foldLeft((d, y, List[Double]())) {
      case ((resDList, resYList, resCentroid), j) =>

        val (dList, yList) = ((data zip resDList) zip resYList).map {
          case ((dataList, dValue), yValue) =>
            val dist = distance match {
              case EUCLIDEAN => NumericFunctions.squaredDistance(dataList, centroid)
              case EUCLIDEAN_MISSING_VALUES => NumericFunctions.squaredDistanceWithMissingValues(dataList, centroid)
              case JENSEN_SHANNON_DIVERGENCE => NumericFunctions.jensenShannonDivergence(dataList, centroid)
            }

            if (dist < dValue) {
              (dist, j - 1)
            } else (dValue, yValue)
        }.unzip

        val cutoff: Double = Math.random() * dList.sum

        val (_, index) = dList.foldLeft((0.0, 0)) {
          case ((cost, index), dValue) =>
            val costSum = cost + dValue
            if (costSum >= cutoff) {
              (cost, index)
            } else (cost, index + 1)
        }
        (dList, yList, data(index))
    }

    val (_, yList) = ((data zip ds) zip ys).map {
      case ((dataList, dValue), yValue) =>
        val dist = distance match {
          case EUCLIDEAN => NumericFunctions.squaredDistance(dataList, centroids)
          case EUCLIDEAN_MISSING_VALUES => NumericFunctions.squaredDistanceWithMissingValues(dataList, centroids)
          case JENSEN_SHANNON_DIVERGENCE => NumericFunctions.jensenShannonDivergence(dataList, centroids)
        }
        if (dist < dValue) {
          (dist, k - 1)
        } else (dValue, yValue)
    }.unzip

    yList
  }

  def lloyd(data: List[List[Double]], k: Int, maxIter: Int, runs: Int) = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k)
    }

    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter)
    }

    if (runs <= 0) {
      throw new IllegalArgumentException("Invalid number of runs: " + runs)
    }

    val best = lloyd(data, k, maxIter)

    (0 to runs-1).foldLeft(best){
      case (prevKmeans, _) =>
        val kmeans = lloyd(data, k, maxIter)
        if(kmeans.distortion < prevKmeans.distortion){
          kmeans
        } else{
          prevKmeans
        }
    }
  }

  def lloyd(data: List[List[Double]], k: Int, maxIter: Int) = {
    if (k < 2) {
      throw new IllegalArgumentException("Invalid number of clusters: " + k);
    }
    if (maxIter <= 0) {
      throw new IllegalArgumentException("Invalid maximum number of iterations: " + maxIter);
    }
    val initialDistortion = Double.MaxValue
    val y = seed(data, k, EUCLIDEAN)
    val (resY, distortion, _) = (0 to maxIter -1).foldLeft((y, initialDistortion, true)){
      case ((prevYs, distortion, isMore), _) =>
        if(isMore){
          val (newCentroids, _) = calculateCentroidsAndSize(k, prevYs, data)
          val (ys, wcss) = asyncSquareDistance(data, newCentroids, prevYs)
          if (distortion <= wcss) {
            (ys, distortion, false)
          } else {
            (ys, wcss, isMore)
          }
        } else (prevYs, distortion, isMore)
    }
    val (finalCentroids, finalSize) = calculateCentroidsAndSize(k, resY, data)

    new KMeans(k, resY, finalSize, distortion, finalCentroids)

  }

  //TODO: as you can see the data is grouped to some chunks. We need to find a way to break the data into pieces
  //and then apply parallelism in it.
  private def asyncSquareDistance(data: List[List[Double]], centroids: List[List[Double]], y: List[Int]): (List[Int], Double) = {
    val numThreads = Runtime.getRuntime.availableProcessors * 2
    val numPieces = data.size / numThreads
    val dataPieces = data.grouped(numPieces).toList
    dataPieces.zipWithIndex.foldLeft(y, 0.0){
      case ((partitionY, partitionWCSS), (pieces, partitionIdx)) =>

        pieces.zipWithIndex.foldLeft((partitionY, partitionWCSS)){
          case (((prevYS, wcss)), (piece, pieceIdx)) =>
           val (newNearest, newYs) = centroids.zipWithIndex.foldLeft((Double.MaxValue, prevYS)){
              case ((nearest, ys), (centroid, idx)) =>
                val dist = NumericFunctions.squaredDistance(piece, centroid)
                if(nearest > dist) {
                  val newYs = y.patch(pieceIdx + (partitionIdx * numPieces), Seq(idx), 1)
                  (dist, newYs)
                } else (nearest, ys)
            }
            (newYs, wcss + newNearest)
        }
    }
  }

  private def calculateCentroidsAndSize(k: Int, y: List[Int], data: List[List[Double]]) = {
    val d = data(0).length
    val initialSize: List[Int] = (0 to k - 1).toList.map(_ => 0)
    val initialCentroids = (0 to k - 1).toList.map(_ => (0 to d - 1).toList.map(_ => 0.0))
    val initialND = initialCentroids
    val reinitializedSize = initialSize.zipWithIndex.map{ case (size, idx) => size + y.filter(_ == idx).size }
    val (reinitializedCentroids, _) = (y zip data).map {
      case (yValue: Int, dataList: List[Double]) =>
        dataList.zipWithIndex.map {
          case (dt, idx) =>
            val centroid =  initialCentroids(yValue)(idx) + dt
            val nd = initialND(yValue)(idx) + 1
            (centroid / nd, nd)
        }.unzip
    }.unzip

    (reinitializedCentroids, reinitializedSize)
  }


}

trait ClusteringDistance

case object EUCLIDEAN extends ClusteringDistance

case object EUCLIDEAN_MISSING_VALUES extends ClusteringDistance

case object JENSEN_SHANNON_DIVERGENCE extends ClusteringDistance