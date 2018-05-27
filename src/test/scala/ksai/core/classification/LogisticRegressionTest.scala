package ksai.core.classification

import ksai.data.parser.{ARFFParser, DelimitedParser}
import ksai.training.validation.LOOCV
import org.scalatest.{Matchers, WordSpec}

class LogisticRegressionTest extends WordSpec with Matchers{

  "LogisticRegression" should {

    "be able to test of learn method with binary output" in {
      println("binary iris")

      val trainARFF = getClass.getResource("/iris.arff").getPath
      val trainData = ARFFParser.parse(trainARFF)
      val x = trainData.data.toArray
      val y = trainData.getNumericTargets.toArray

      for(i <- y.indices){
        if(y(i) == 2){
          y(i) = 1
        } else{
          y(i) = 0
        }
      }

      val n = x.length

      val loocv = LOOCV(n)

      val error = ( 0 until n).foldLeft(0){(err, i) =>
        val trainX: Array[Array[Double]] = LOOCV.slice(x, loocv.train(i).toArray).toArray
        val trainY: Array[Int] = LOOCV.slice(y, loocv.train(i).toArray).toArray
        val logisticRegression: LogisticRegression = LogisticRegression(trainX, trainY)
        println(s"Updated weights are: ${logisticRegression.linearWeights.toList}")
        println(s"With log likelihood: ${logisticRegression.logLikelihood}")
        if (y(loocv.test(i)) != logisticRegression.predict(x(loocv.test(i)))){
          err + 1
        } else {
          err
        }
      }

      println("Logistic Regression error = " + error)
      assert(error == 3)
    }

    "be able to test of learn method" in {
      println("iris test")

      val trainARFF = getClass.getResource("/iris.arff").getPath
      val trainData = ARFFParser.parse(trainARFF)
      val x = trainData.data.toArray
      val y = trainData.getNumericTargets.toArray

      val n = x.length

      val loocv = LOOCV(n)

      val error = ( 0 until n).foldLeft(0){(err, i) =>
        val trainX: Array[Array[Double]] = LOOCV.slice(x, loocv.train(i).toArray).toArray
        val trainY: Array[Int] = LOOCV.slice(y, loocv.train(i).toArray).toArray
        val logisticRegression: LogisticRegression = LogisticRegression(trainX, trainY)
        if (y(loocv.test(i)) != logisticRegression.predict(x(loocv.test(i)))){
          err + 1
        } else {
          err
        }
      }

      println("Logistic Regression error = " + error)
      assert(error == 3)
    }

    "be able to test segment" in {
      println("segment test")

      val trainARFF = getClass.getResource("/segment-challenge.arff").getPath
      val testARFF = getClass.getResource("/segment-test.arff").getPath
      val trainData = ARFFParser.parse(trainARFF)
      val testData = ARFFParser.parse(testARFF)

      val trainX = trainData.data.toArray
      val trainY = trainData.getNumericTargets.toArray

      val testX = testData.data.toArray
      val testY = testData.getNumericTargets.toArray

      val logit = LogisticRegression(trainX, trainY, 0.05, 1E-3, 1000)

      val error = testX.indices.foldLeft(0){ (err, i) =>
        if (logit.predict(testX(i)) != testY(i)) err + 1 else err
      }

      println(f"Segment error rate = ${100.0 * error / testX.length}%.2f%%%n")
      assert(error == 48)
    }

    //TODO: Need to fix the parser for USPS/zip files
    "be able to test USPS" in {
      println("USPS")
      pending
      val zipTrainingPath = getClass.getResource("/zip.train").getPath
      val zipTestPath = getClass.getResource("/zip.test").getPath
      val trainData = DelimitedParser.parse(zipTrainingPath)
      val testData = DelimitedParser.parse(zipTestPath)

      val trainX = trainData.data.toArray
      val trainY = trainData.getNumericTargets.toArray

      val testX = testData.data.toArray
      val testY = testData.getNumericTargets.toArray

      val logit = LogisticRegression(trainX, trainY, 0.03, 1E-3, 1000)

      val error = testX.indices.foldLeft(0){ (err, i) =>
        if (logit.predict(testX(i)) != testY(i)) err + 1 else err
      }

      println(f"Segment error rate = ${100.0 * error / testX.length}%.2f%%%n")
      assert(error == 48)
    }


  }

}

