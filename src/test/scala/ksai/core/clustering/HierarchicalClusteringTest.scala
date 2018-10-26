package ksai.core.clustering

import ksai.core.cluster.HierarchicalClustering
import ksai.core.cluster.linkage.{CompleteLinkage, SingleLinkage}
import ksai.data.parser.DelimitedParser
import ksai.training.validation.AdjustRandIndex
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class HierarchicalClusteringTest extends WordSpec with Matchers with BeforeAndAfterAll {

  val zipTrainingPath = getClass.getResource("/zip.train").getPath
  val delimitedParserRefactored = new DelimitedParser(0)
  val trainData = delimitedParserRefactored.parse(zipTrainingPath)

  val trainX: Array[Array[Double]] = trainData.data.toArray
  val trainY: Array[Int] = trainData.getNumericTargets.toArray

  val n = trainX.length

  val proximity = new Array[Array[Double]](n)

  (0 until n).foreach{ itr =>
    proximity(itr) = new Array[Double](itr + 1)
    (0 until itr).foreach{ innerItr =>
      proximity(itr)(innerItr) = Math.distance(trainX(itr), trainX(innerItr))
    }
  }

  "HierarchicalClustering" should {
    "be able to test singleLinkage" in {
      val hc = HierarchicalClustering(SingleLinkage(proximity))

      val label = hc.partition(10)
      val r = AdjustRandIndex.measureRand(trainY, label)
      val r2 = AdjustRandIndex.measure(trainY, label)
      println(s"SingleLinkage rand index = ${100.0 * r}\tadjusted rand index = ${100.0 * r2}")

      assert(r > 0.1)
    }

    "be able to test Complete linkage" in {
      val hc = HierarchicalClustering(CompleteLinkage(proximity))

      val label = hc.partition(10)
      val r = AdjustRandIndex.measureRand(trainY, label)
      val r2 = AdjustRandIndex.measure(trainY, label)
      println(s"CompleteLinkage rand index = ${100.0 * r}\tadjusted rand index = ${100.0 * r2}")

      assert(r > 0.75)
    }
  }


}


object Math {


  /**
    * The Euclidean distance.
    */
  def distance(x: Array[Double], y: Array[Double]): Double = sqrt(squaredDistance(x, y))

  /**
    * The squared Euclidean distance.
    */
  def squaredDistance(x: Array[Double], y: Array[Double]): Double = {
    if (x.length != y.length) throw new IllegalArgumentException("Input vector sizes are different.")
    var sum = 0.0
    x.indices.foreach{ itr =>
      sum = sum + sqr(x(itr) - y(itr))
    }
    sum
  }

  /**
    * Returns number * number.
    */
  def sqr(number: Double): Double = number * number


  /**
    * Returns the correctly rounded positive square root of a double value.
    */
  def sqrt(a: Double): Double = java.lang.Math.sqrt(a)

}

