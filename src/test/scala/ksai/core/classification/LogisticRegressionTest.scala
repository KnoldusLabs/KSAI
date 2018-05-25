package ksai.core.classification

import ksai.data.parser.ARFFParser
import ksai.training.validation.LOOCV
import org.scalatest.{FlatSpec, Matchers}

class LogisticRegressionTest extends FlatSpec with Matchers {

  it should "test the learn method with binary iris" in {
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
      val trainX = sliceX(x, loocv.train(i).toArray)
      val trainY = sliceY(y, loocv.train(i).toArray)
      val logisticRegression = LogisticRegression(trainX, trainY)
      if (y(loocv.test(i)) != logisticRegression.predict(x(loocv.test(i)))){
        err + 1
      } else {
        err
      }
    }

    println("Logistic Regression error = " + error)
    assert(error == 3)
  }

  /*
  it should "test the learn method" in {
    val trainARFF = getClass.getResource("/iris.arff").getPath
    val trainData = ARFFParser.parse(trainARFF)
    val x = trainData.data.toArray
    val y = trainData.getNumericTargets.toArray

    val n = x.length

    val loocv = LOOCV(n)

    val error = ( 0 until n).foldLeft(0){(err, i) =>
      val trainX = sliceX(x, loocv.train(i).toArray)
      val trainY = sliceY(y, loocv.train(i).toArray)
      val logisticRegression = LogisticRegression(trainX, trainY)
      if (y(loocv.test(i)) != logisticRegression.predict(x(loocv.test(i)))){
        err + 1
      } else {
        err
      }
    }

    println("Logistic Regression error = " + error)
    assert(error == 3)
  }*/

  def sliceX(data:Array[Array[Double]], index: Array[Int]): Array[Array[Double]] = {
    index.indices.map{ itr =>
      data(index(itr))
    }.toArray
  }

  def sliceY(data:Array[Int], index: Array[Int]): Array[Int] = {
    index.indices.map{ itr =>
      data(index(itr))
    }.toArray
  }

}
