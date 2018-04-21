package ksai.core.classification.knn

import ksai.data.parser.{ARFF, ARFFParser}
import ksai.training.validation.ValidationImplicits
import org.scalatest.{Matchers, WordSpec}

class KNNSpec extends WordSpec with Matchers with ValidationImplicits {

  "KNN" should {

    "predict with k nearest neighbors" in {

      val arffFile: String = getClass.getResource("/iris.arff").getPath
      val arff: ARFF[String] = ARFFParser.parse(arffFile)

      val data: Array[Array[Double]] = arff.data.toArray
      val results: Array[Int] = arff.getNumericTargets.toArray

      //KNN with K = 3
      val knn3: KNN = KNN.learn(data, results, 3)
      var error = 0
      (0 until data.length).map{ i =>
        val result = knn3.predict(data(i))
        if(result != results(i)){
          error = error + 1
        }
      }
      println("\n\nKNN with K = 3 ======>  ERROR: " + error)

      //KNN with K = 5
      val knn5: KNN = KNN.learn(data, results, 5)
      error = 0
      (0 until data.length).map{ i =>
        val result = knn5.predict(data(i))
        if(result != results(i)){
          error = error + 1
        }
      }
      println("\n\nKNN with K = 5 ======>  ERROR: " + error)

      //KNN with K = 7
      val knn7: KNN = KNN.learn(data, results, 7)
      error = 0
      (0 until data.length).map{ i =>
        val result = knn7.predict(data(i))
        if(result != results(i)){
          error = error + 1
        }
      }
      println("\n\nKNN with K = 7 ======>  ERROR: " + error)

      //KNN with K = 10
      val knn10: KNN = KNN.learn(data, results, 7)
      error = 0
      (0 until data.length).map{ i =>
        val result = knn10.predict(data(i))
        if(result != results(i)){
          error = error + 1
        }
      }
      println("\n\nKNN with K = 10 ======> ERROR: " + error)
      println()

      assert(error < 10)
    }

  }

}
