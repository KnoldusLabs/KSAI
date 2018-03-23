package ksai.core.clustering

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import ksai.core.cluster.KMeans
import ksai.data.parser.{Delimited, DelimitedParser}
import ksai.training.validation.{AdjustRandIndex, ValidationImplicits}
import ksai.validation.RandIndex
import org.scalatest.{AsyncFlatSpec, Matchers}


class KMeansTest extends AsyncFlatSpec with Matchers with ValidationImplicits {


  val mean1 = DenseVector(1.0, 1.0, 1.0)
  val covariance1 = DenseMatrix((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0))
  val mean2 = DenseVector(-2.0, -2.0, -2.0)
  val covariance2 = DenseMatrix((1.0, 0.3, 0.8), (0.3, 1.0, 0.5), (0.8, 0.5, 1.0))
  val mean3 = DenseVector(4.0, 2.0, 3.0)
  val covariance3 = DenseMatrix((1.0, 0.8, 0.3), (0.8, 1.0, 0.5), (0.3, 0.5, 1.0))
  val mean4 = DenseVector(3.0, 5.0, 1.0)
  val covariance4 = DenseMatrix((1.0, 0.5, 0.5), (0.5, 1.0, 0.5), (0.5, 0.5, 1.0))

  val multivariantGaussian1 = MultivariateGaussian(mean1, covariance1)
  val multivariantGaussian2 = MultivariateGaussian(mean2, covariance2)
  val multivariantGaussian3 = MultivariateGaussian(mean3, covariance3)
  val multivariantGaussian4 = MultivariateGaussian(mean4, covariance4)

  val (data1, label1) = (1 to 20000).toList.map(_ => (multivariantGaussian1.draw(), 0)).unzip
  val (data2, label2) = (1 to 20000).toList.map(_ => (multivariantGaussian2.draw(), 1)).unzip
  val (data3, label3) = (1 to 20000).toList.map(_ => (multivariantGaussian3.draw(), 2)).unzip
  val (data4, label4) = (1 to 20000).toList.map(_ => (multivariantGaussian4.draw(), 3)).unzip

  val data: List[List[Double]] = (data1 ::: data2 ::: data3 ::: data4).map(_.data.toList)
  val label = label1 ::: label2 ::: label3 ::: label4

  "K-Means" should "be able to apply separate files validation with LMS USPS" in {
    pending
    val zipTraingPath = getClass.getResource("/zip.train").getPath
    val zipTestPath = getClass.getResource("/zip.test").getPath
    val delimited: Delimited[String] = DelimitedParser.parse(zipTraingPath)
    val delimitedTest: Delimited[String] = DelimitedParser.parse(zipTestPath)
    val inputNodesNum = delimited.data.head.size

    KMeans(delimited.data.map(_.toList), 10, 10, 1).map {
      case kmeans =>
        val r1 = RandIndex.measure(delimited.getNumericTargets, kmeans.y)
        println(".............done")
        assert(r1 > 0.85)
    }

  }

  "K-Means" should "be able to applicable to lloyd" in {
    println("Lloyd 4");
    KMeans(data, 4, 100).map {
      case kmeans =>
        val r = AdjustRandIndex.measure(label, kmeans.y)
        val r2 = AdjustRandIndex.measureRand(label, kmeans.y)
        println(s"Training rand index = ${100.0 * r} adjusted rand index = ${100.0 * r2}")
        assert(r > r2)
    }
  }

  "K-Means" should "be able to applicable to lloyd with 64" in {
    println("Lloyd 4");
    KMeans(data, 64, 100).map {
      case kmeans =>
        val r = AdjustRandIndex.measure(label, kmeans.y)
        val r2 = AdjustRandIndex.measureRand(label, kmeans.y)
        println(s"Training rand index = ${100.0 * r} adjusted rand index = ${100.0 * r2}")
        assert(r > r2)
    }
  }


}
