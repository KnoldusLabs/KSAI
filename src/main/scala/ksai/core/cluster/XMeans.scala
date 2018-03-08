package ksai.core.cluster

import ksai.multithreading.{GenerateKMeansWithRuns, KMeansFactory}
import ksai.util.NumericFunctions
import akka.pattern._
import akka.util.Timeout

import scala.concurrent.Future
import ksai.multithreading.KAsyncExec._
import akka.util.Timeout
import scala.concurrent.duration._


case class XMeans(
                   kMeans: KMeans
                 ){
  def k: Int = kMeans.k
  def y: List[Int] = kMeans.y
  def size: List[Int] = kMeans.size
  def distortion: Double = kMeans.distortion
  def centroids: List[List[Double]] = kMeans.centroids

  def predict(x: List[Double]): Int = kMeans.predict(x)
}

object XMeans {

  implicit val timeout = Timeout(2*60*60 seconds)

  def apply(data: List[List[Double]], kmax: Int): Future[XMeans] = {
    if (kmax < 2) {
      throw new IllegalArgumentException("Invalid parameter kmax = " + kmax)
    }

    val n = data.length
    val d = data(0).length

    val k = 1
    val size = (0 to k - 1).toList.map(_ => 0)
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
    println(s"X-Means distortion with $k clusters: $distortion")

    val bbd = BBDKDTree(data)
    val defaultKMeans = KMeans(k, y, size, distortion, centroids)
    recursiveBIC(data, defaultKMeans, bbd, kmax).map(kmeans => new XMeans(kmeans))
  }

  private def recursiveBIC(data: List[List[Double]], defaultKMeans: KMeans, bbd: BBDKDTree, kmax: Int): Future[KMeans] = {
    getKMeans(data, defaultKMeans, bbd, kmax).flatMap{
      kmeans => if(defaultKMeans.k >= kmax){
        Future.successful(defaultKMeans)
      } else {
          recursiveBIC(data, kmeans, bbd, kmax)
      }
    }
  }


  private def getKMeans(data: List[List[Double]], defaultKMeans: KMeans, bbd: BBDKDTree, kmax: Int): Future[KMeans] = {
    val columnCount = data(0).size
    val labelMap = defaultKMeans.y.zipWithIndex.groupBy { case (value, index) => value }
    println(s"Labels ${defaultKMeans.k}>>>>>>>>>>> ${defaultKMeans.y.distinct}")
    val kmeansGenerator = KMeansFactory.getKMeansGeneratorActor()
    implicit val system = KMeansFactory.system
    val kmeansScores: List[Future[(Option[KMeans], Option[Double])]] = ((0 to defaultKMeans.k - 1).toList.zip(defaultKMeans.centroids)).map {
      case (kValue, centroid) =>
        labelMap.get(kValue) match {
          case Some(subsetList) =>
            val subset: List[List[Double]] = subsetList.map { case (value, index) => data(index) }
            val bbdTree = if(defaultKMeans.k == 1){
              Some(bbd)
            } else None
            val actorResult = (kmeansGenerator ? GenerateKMeansWithRuns(subset, 2, 100, 4, bbdTree)).map {
              case kmeans: KMeans =>
                println(s"after actor hit ${kmeans.k}>>>>>>>>>>> ${kmeans.y.distinct}")
                val wcss = subset.foldLeft(0.0) {
                  case (result, subRow) => result + NumericFunctions.squaredDistance(subRow, centroid)
                }
                val newBIC = bic(2, subsetList.size, columnCount, kmeans.distortion, kmeans.size)
                val oldBIC = bic(subsetList.size, columnCount, wcss)
                println(s"New BIC $newBIC        Old BIC $oldBIC       ${newBIC - oldBIC}")
                println(s"size ${subsetList.size}  columnCount ${columnCount} ... distortion ${kmeans.distortion} .... kmeans size ${kmeans.size}")
                (Some(kmeans), Some(newBIC - oldBIC))
            }
            println(s"....kcount $kValue")
            actorResult
          case None => Future.successful((None, None))
        }
    }

    Future.sequence(kmeansScores).map {
      case kmeansScores =>
        val (kmeansList, scores) = kmeansScores.map {
          case (Some(km), Some(sc)) => Some((km, sc))
          case _ => None
        }.flatten.sortWith {
          case ((_, sc1), (_, sc2)) => sc1 < sc2
        }.unzip
        println(s"..............${scores}")
        val centers = scores.zipWithIndex.filter { case (score, index) => score <= 0.0 }.map {
          case (score, index) => defaultKMeans.centroids(index)
        }
        val newCenters = (0 to defaultKMeans.k - 1).toList.foldRight(centers) {
          case (kCount, result) => if (scores(kCount) > 0) {
            if (result.size + kCount - centers.size + 1 < kmax) {
              val countMeans = kmeansList(kCount)
              println(s"...centroids of countmeans ${countMeans.centroids}")
              (centers :+ countMeans.centroids(0)) :+ countMeans.centroids(1)
            } else {
              centers :+ defaultKMeans.centroids(kCount)
            }
          } else {
            centers
          }
        }
        println(s".....${centers.size}.......$centers")
        if (newCenters.size == defaultKMeans.k) {
          defaultKMeans
        } else {
          val newK = newCenters.size
          val sums = (0 to newK - 1).toList.map(_ => (0 to newCenters.head.size - 1).toList.map(_ => 0.0))
          val size = (0 to newK - 1).toList.map(_ => 0)

          val updatedCentroids = newCenters ::: defaultKMeans.centroids.drop(newK)
          val distortion = Double.MaxValue

          val (finalDistortion, finalCentroids, finalSums, finalSize, finalLabels, _) = (0 to 99).toList.foldLeft(
            (distortion, updatedCentroids, sums, size, defaultKMeans.y, false)) {
            case ((distort, upCents, sms, sz, lbls, isStopped), _) =>
              if (isStopped) {
                (distort, upCents, sms, sz, lbls, isStopped)
              } else {
                val (newDistortion, clusteredSums, clusteredSize, clusteredLabels) = bbd.clustering(upCents, sms, sz, lbls)
                val clusteredCentroids = updatedCentroids.take(newK).zipWithIndex.map {
                  case (cent, index) => if (clusteredSize(index) > 0) {
                    (clusteredSums(index).zip(clusteredSize)).map { case (sum, sz) => sum / sz }
                  } else {
                    cent
                  }
                }
                if (distort <= newDistortion) {
                  (newDistortion, clusteredCentroids, clusteredSums, clusteredSize, clusteredLabels, true)
                } else {
                  (newDistortion, clusteredCentroids, clusteredSums, clusteredSize, clusteredLabels, false)
                }
              }
          }

          new KMeans(newK, finalLabels, finalSize, finalDistortion, finalCentroids)
        }
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

