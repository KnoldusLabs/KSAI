package ksai.core.classification.knn

import breeze.linalg.DenseMatrix
import ksai.data.parser.{ARFF, ARFFParser}
import ksai.training.validation.ValidationImplicits
import org.scalatest.{Matchers, WordSpec}

class KNNSpec extends WordSpec with Matchers with ValidationImplicits {

  "KNN" should {

    "build itself" in {

      val arffFile: String = getClass.getResource("/iris.arff").getPath
      val arff: ARFF[String] = ARFFParser.parse(arffFile)

      val data: Array[Array[Double]] = arff.data.toArray
      val results: Array[Int] = arff.getNumericTargets.toArray

      println("***DATA**********" + data.toList.map(_.toList))
      println("***DATA SIZE**********" + data.toList.size)
      println("***RESULT**********" + results.toList)
      println("***RESULT SIZE**********" + results.toList.size)

      val knn: KNN = KNN.learn(data, results, 3)

      println("****************knn.knn------------" + knn.knn)

      var error = 0

      (0 until data.length).drop(50).take(100).map{ i =>
        val result = knn.predict(data(i))
        println("\n\n\nFOR DATA......>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" + data(i).toList)
        println("EXPECTED RESULT......>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" + results(i))
        println("REULT FOUND......>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" + result)
        if(result != results(i)){
          error = error + 1
        }
      }

      println("*********5 NN ERROR: " + error)

      /*error = 0
      var i = 0
      while ( {i < x.length}) {
        if (knn.predict(x(i)) != y(i)) error += 1 {
          i += 1
          i - 1
        }
      }*/

      assert(error < 10)
    }

  }

}
