package ksai.core.classification

import breeze.linalg.DenseMatrix
import ksai.data.parser.{ARFF, ARFFParser, Delimited, DelimitedParser}
import ksai.training.validation.ValidationImplicits
import ksai.validation.LOOCV
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer


class LDATest extends FlatSpec with Matchers with ValidationImplicits {

  "LDA" should "load the values easily" in {
    val arffFile = getClass.getResource("/iris.arff").getPath
    val arff = ARFFParser.parse(arffFile)
    val n = arff.data.length
    val loocv = LOOCV(n)
    var error = 0
    val posteriori = ArrayBuffer[Double]((0 to 2).map(_ => 0.0):_*)
    val bufferedData = arff.data.map(_.to[ArrayBuffer]).to[ArrayBuffer]
    val bufferedNumericTargets = arff.getNumericTargets.to[ArrayBuffer]
    (0 to n-1).foreach{ index =>
      val trainx: ArrayBuffer[ArrayBuffer[Double]] = LOOCV.slice(bufferedData, loocv.train(index))
      val trainy: ArrayBuffer[Int] = LOOCV.sliceY(bufferedNumericTargets, loocv.train(index))
      val lda = LDA(trainx, trainy)

      if (arff.getNumericTargets(loocv.test(index)) != lda.predict(bufferedData(loocv.test(index)), posteriori))
        error = error + 1
    }

//    val lda = LDA(arff.data.map(_.to[ArrayBuffer]).to[ArrayBuffer], arff.getNumericTargets.to[ArrayBuffer])
//    lda.predict(arff.data.head.to[ArrayBuffer], ArrayBuffer[Double](0.0, 0.0, 0.0))
//    assert(lda.p > lda.k)
    assert(error == 100)
  }

  it should "workable on traing and test dataset" in {
    val zipTraingPath = getClass.getResource("/zip.train").getPath
    val zipTestPath = getClass.getResource("/zip.test").getPath
    val delimited: Delimited[String] = DelimitedParser.parse(zipTraingPath)
    val delimitedTest: Delimited[String] = DelimitedParser.parse(zipTestPath)
    val lda = LDA(delimited.data.map(_.to[ArrayBuffer]).to[ArrayBuffer], delimited.getNumericTargets.to[ArrayBuffer])
    var error = 0
    (0 to delimitedTest.data.length -1).foreach{ index =>
      if (lda.predict(delimitedTest.data(index).to[ArrayBuffer]) != delimitedTest.getNumericTargets(index)) {
        error = error + 1
      }
    }
    assert(error == 22)
  }

}
