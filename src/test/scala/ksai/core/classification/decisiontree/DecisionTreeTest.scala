package ksai.core.classification.decisiontree

import ksai.data.parser.ARFFParser
import ksai.training.validation.LOOCV
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ArrayBuffer

class DecisionTreeTest extends WordSpec with Matchers {

  "Decision Tree" should {
    "be testable" in {
      val weatherFile = getClass.getResource("/iris.arff").getPath
      val weather = ARFFParser.parse(weatherFile)
      val x = weather.data.toArray.map(_.to[ArrayBuffer]).to[ArrayBuffer]
      val y = weather.getNumericTargets.to[ArrayBuffer]

      val n = x.length

      val loocv = LOOCV(n)

      val error = (0 until n).foldLeft(0) { (err, i) =>
        val trainX = LOOCV.slice(x, loocv.train(i)).toArray.map(_.toArray)
        val trainY: Array[Int] = LOOCV.sliceY(y, loocv.train(i)).toArray
        val decisionTree = DecisionTree(trainX, trainY, 4)
        if (y(loocv.test(i)) != decisionTree.predict(x(loocv.test(i)).toArray)) {
          err + 1
        } else {
          err
        }
      }

      println("Decision Tree error = " + error)
      assert(error == 7)
    }
  }
}
