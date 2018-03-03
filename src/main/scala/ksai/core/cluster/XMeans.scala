package ksai.core.cluster

import ksai.multithreading.{GenerateKMeansWithRuns, KMeansFactory}
import ksai.util.NumericFunctions
import akka.pattern._

import scala.concurrent.Future

/**
  * BICClusteringAutomation algorithm, an extended K-Means which tries to
  * automatically determine the number of clusters based on BIC scores.
  * Starting with only one cluster, the X-Means algorithm goes into action
  * after each run of K-Means, making local decisions about which subset of the
  * current centroids should split themselves in order to better fit the data.
  * The splitting decision is done by computing the Bayesian Information
  * Criterion (BIC).
  *
  * <h2>References</h2>
  * <ol>
  * <li> Dan Pelleg and Andrew Moore. X-means: Extending K-means with Efficient Estimation of the Number of Clusters. ICML, 2000. </li>
  * </ol>
  *
  * @see KMeans
  * @see GMeans
  */
object XMeans {

  def apply(data: List[List[Double]], kmax: Int): XMeans = {
    if (kmax < 2) {
      throw new IllegalArgumentException("Invalid parameter kmax = " + kmax)
    }

    val n = data.length
    val d = data(0).length

    val k = 1
    val size = (0 to k - 1).toList.map(_ => 0)
    val size[0] = n
    val y = (0 to n - 1).toList.map(_ => 0)
    val centroidRow: List[Double] = data.tail.foldLeft(data.head) {
      case (result, row) => (result zip row).map { case (a, b) => a + b }
    }.map(cent => cent / n)
    val centroids = centroidRow +: (1 to k - 1).toList.map {
      case (k) => (0 to d - 1).toList.map(_ => 0.0)
    }

    val wcss = data.foldLeft(0.0) {
      case (result, row) => result + NumericFunctions.squaredDistance(row, centroidRow)
    }

    val distortion = wcss
    println(String.format("X-Means distortion with %d clusters: %.5f", k, distortion))

    val bbd = BBDKDTree(data)


  }

  private def findKRecursively(data: List[List[Double]], y: List[Int], k: Int, columnCount: Int, centroids: List[List[Double]], bbd: BBDKDTree) = {
    val labelMap = y.zipWithIndex.groupBy { case (value, index) => value }
    val kmeansGenerator = KMeansFactory.getKMeansGeneratorActor()
    val kmeansScores = (0 to k - 1).zip(centroids).toList.map {
      case (kValue, centroid) =>
        labelMap.get(kValue) match {
          case Some(subsetList) =>
            val subset = subsetList.map { case (value, index) => data(index) }
            (kmeansGenerator ? GenerateKMeansWithRuns(subset, 2, 100, 4)).map {
              case kmeans: KMeans =>
                val wcss = subset.foldLeft(0.0) {
                  case (result, subRow) => result + NumericFunctions.squaredDistance(subRow, centroid)
                }
                val newBIC = bic(2, subsetList.size, columnCount, kmeans.distortion, kmeans.size)
                val oldBIC = bic(subsetList.size, columnCount, wcss)
                (Some(kmeans), Some(newBIC - oldBIC))
            }
          case None => Future.successful((None, None))
        }
    }

    Future.sequence(kmeansScores).map {
      case kmeansScores =>
        val (kmeansList, scores) = kmeansScores.unzip
        val centers = scores.zipWithIndex.filter { case (score, index) => score.map(s => s <= 0.0).getOrElse(false) }.map { case (score, index) => centroids(index) }
        scores.flatten.map
    }
  }

  private def bic(n: Int, d: Int, distortion: Double) = {
    val variance = distortion / (n - 1)
    val p1 = -n * NumericFunctions.log2Pi()
    val p2 = -n * d * Math.log(variance)
    val p3 = -(n - 1)
    val L = (p1 + p2 + p3) / 2
    val numParameters = d + 1

    L - 0.5 * numParameters * Math.log(n)
  }


  private def bic(k: Int, n: Int, d: Int, distortion: Double, clusterSize: List[Int]) = {
    val variance = distortion / (n - k)

    val L = (0 to k - 1).toList.foldLeft(0.0) {
      case (result, kValue) => result + logLikelihood(k, n, clusterSize(kValue), d, variance)
    }
    val numParameters = k + k * d
    L - 0.5 * numParameters * Math.log(n)
  }


  private def logLikelihood(k: Int, n: Int, ni: Int, d: Int, variance: Double) = {
    val p1 = -ni * NumericFunctions.log2Pi()
    val p2 = -ni * d * Math.log(variance)
    val p3 = -(ni - k)
    val p4 = ni * Math.log(ni)
    val p5 = -ni * Math.log(n)
    val loglike = (p1 + p2 + p3) / 2 + p4 + p5
    loglike
  }


}

case class XMeans(
                   kMeans: KMeans
                 )
