package ksai.core.regression

import breeze.linalg.DenseMatrix
import ksai.data.parser.{ARFF, ARFFParser, Delimited, DelimitedParser}
import ksai.training.validation.ValidationImplicits
import org.scalatest.{FlatSpec, Matchers}
import breeze.stats._
import breeze.linalg._


class NeuralNetworkTest extends FlatSpec with Matchers with ValidationImplicits {

  "A Network for Regression" should "be able to apply k-fold validation with Logistic Sigmoid" in {
    val arffFile = getClass.getResource("/cpu.arff").getPath
    val arff: ARFF[String] = ARFFParser.parse(arffFile)
    val inputNodesNum = arff.data.head.size
    val allNetworkAndError = arff.kFoldRegression(13) {
      case (trainingSet, target, validationSet, validationTarget) => {
        val network = Network(inputNodesNum, 10, 10, 1)
        val trainedNetwork = network.learn(DenseMatrix(trainingSet: _*), target.toArray)
        val errorSum: List[Double] = (validationSet zip validationTarget).map {
          case (arr, target) =>
            val r = target - trainedNetwork.predict(arr)
            r * r
        }

        (errorSum.sum, trainedNetwork)
      }
    }

    val totalErrors: List[Double] =  allNetworkAndError.map { case (error: Double, _) => error }
    val completelyPassed = totalErrors.sum / arff.data.size
    println(completelyPassed)
    assert(completelyPassed <= 2.0)
  }

  it should "be able to apply k-fold validation with TANH" in {
    val arffFile = getClass.getResource("/cpu.arff").getPath
    val arff: ARFF[String] = ARFFParser.parse(arffFile)
    val inputNodesNum = arff.data.head.size
    val allNetworkAndError = arff.kFoldRegression(13) {
      case (trainingSet, target, validationSet, validationTarget) => {
        val network = Network(TANH, inputNodesNum, 10, 10, 1)
        val trainedNetwork = network.learn(DenseMatrix(trainingSet: _*), target.toArray)
        val errorSum: List[Double] = (validationSet zip validationTarget).map {
          case (arr, target) =>
            val r = target - trainedNetwork.predict(arr)
            r * r
        }

        (errorSum.sum, trainedNetwork)
      }
    }

    val totalErrors: List[Double] =  allNetworkAndError.map { case (error: Double, _) => error }
    val completelyPassed = totalErrors.sum / arff.data.size
    println(completelyPassed)
    assert(completelyPassed <= 1.1)
  }



}
