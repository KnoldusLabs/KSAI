package ksai.core.classification

import breeze.linalg.{DenseMatrix, DenseVector}
import ksai.data.parser._
import ksai.training.validation.ValidationImplicits
import org.scalatest.{FlatSpec, Matchers}


class NeuralNetworkTest extends FlatSpec with Matchers with ValidationImplicits {


  "A Network" should "select a Activation functional automatically for given error function" in {
    val net = Network(LeastMeanSquares, 5, 4, 3, 2)
    net.activationFunction should be theSameInstanceAs LogisticSigmoid
  }

  it should "generate random values for weight" in {
    val net = Network(LeastMeanSquares, 5, 4, 3, 2)
    println(net.net.last.weight)
    println()
    println(net.net.last.delta)
  }

  it should "Create a network when passed densematrix and its a temoporary test case" in {
    val arffFile = getClass.getResource("/iris.arff").getPath
    val arff = ARFFParser.parse(arffFile)
    val inputNodesNum = arff.data.head.length
    val network = Network(CrossEntropy, SoftMax, inputNodesNum, 10, 3)
    val trainedNetwork = network.learn(DenseMatrix(arff.data: _*), arff.getNumericTargets.toArray)
    println(trainedNetwork)
    assert(trainedNetwork.learningRate != 0)
  }

  it should "be able to apply k-fold validation with CrossEntropy and SoftMax" in {
    val arffFile = getClass.getResource("/iris.arff").getPath
    val arff: ARFF[String] = ARFFParser.parse(arffFile)
    val inputNodesNum = arff.data.head.length
    val allNetworkAndError = arff.kFoldNN(70) {
      case (trainingSet, target, validationSet, validationTarget) => {
        val network = Network(CrossEntropy, SoftMax, inputNodesNum, 10, 3)
        val trainedNetwork = network.learn(DenseMatrix(trainingSet: _*), target.toArray)
        val errorList = (validationSet zip validationTarget).map {
          case (arr, actualOutput) => trainedNetwork.predict(arr) == actualOutput
        }
        (errorList.filterNot(x => x).length, trainedNetwork)
      }
    }

    val completelyPassed = allNetworkAndError.map { case (error, _) => error }.count(nume => nume == 0)
    println(completelyPassed)
    assert(completelyPassed == 20)
  }

  it should "be able to apply k-fold validation with CrossEntropy and LogisticSigmoid" in {
    val arffFile = getClass.getResource("/iris.arff").getPath
    val arff: ARFF[String] = ARFFParser.parse(arffFile)
    val inputNodesNum = arff.data.head.length
    val allNetworkAndError = arff.kFoldNN(70) {
      case (trainingSet, target, validationSet, validationTarget) => {
        val network = Network(CrossEntropy, LogisticSigmoid, inputNodesNum, 10, 1)
        val trainedNetwork = network.learn(DenseMatrix(trainingSet: _*), arff.getBinaryTargets(target.toArray, 2))
        val errorList = (validationSet zip validationTarget).map {
          case (arr, actualOutput) => trainedNetwork.predict(arr) == actualOutput
        }
        (errorList.filterNot(x => x).length, trainedNetwork)
      }
    }

    val completelyPassed = allNetworkAndError.map { case (error, _) => error }.count(nume => nume == 0)
    println(completelyPassed)
    assert(completelyPassed == 20)
  }

  it should "be able to apply separate files validation" in {
    val arffSegmentChallenge = getClass.getResource("/segment-challenge.arff").getPath
    val arffSegmentTest = getClass.getResource("/segment-test.arff").getPath
    val arff: ARFF[String] = ARFFParser.parse(arffSegmentChallenge)
    val arffTest: ARFF[String] = ARFFParser.parse(arffSegmentTest)
    val inputNodesNum = arff.data.head.length
    val network = Network(CrossEntropy, SoftMax, inputNodesNum, 30, arff.getNumericTargets.max + 1)
    val trainedNetwork = network.learn(DenseMatrix(arff.data: _*), arff.getNumericTargets.toArray)
    val errorList = (arffTest.data zip arffTest.getNumericTargets).map {
      case (arr, actualOutput) => trainedNetwork.predict(arr) == actualOutput
    }
    val (nonErrors, errors) = errorList.partition(isError => isError)

    println(nonErrors.length + " " + errors.length)
    assert(nonErrors.length == 125)
    assert(errors.length == 685)
  }

  it should "be able to apply separate files validation with LMS" in {
    val arffSegmentChallenge = getClass.getResource("/segment-challenge.arff").getPath
    val arffSegmentTest = getClass.getResource("/segment-test.arff").getPath
    val arff: ARFF[String] = ARFFParser.parse(arffSegmentChallenge)
    val arffTest: ARFF[String] = ARFFParser.parse(arffSegmentTest)
    val inputNodesNum = arff.data.head.length
    val network = Network(LeastMeanSquares, LogisticSigmoid, inputNodesNum, 30, arff.getNumericTargets.max + 1)
    val trainedNetwork = network.learn(DenseMatrix(arff.data: _*), arff.getNumericTargets.toArray)
    val errorList = (arffTest.data zip arffTest.getNumericTargets).map {
      case (arr, actualOutput) => trainedNetwork.predict(arr) == actualOutput
    }
    val (nonErrors, errors) = errorList.partition(isError => isError)

    println(nonErrors.length + " " + errors.length)
    assert(nonErrors.nonEmpty)
//    assert(errors.length == 700)
  }

  it should "be able to apply separate files validation with USPS" in {
    pending //TODO:fix me
    val zipTrainingPath = getClass.getResource("/zip.train").getPath
    val zipTestPath = getClass.getResource("/zip.test").getPath
    val delimitedParser = new DelimitedParser(0)
    val delimited = delimitedParser.parse(zipTrainingPath)
    val delimitedTest = delimitedParser.parse(zipTestPath)
    val inputNodesNum = delimited.data.head.length
    val network = Network(CrossEntropy, SoftMax, inputNodesNum, 40, delimited.getNumericTargets.max + 1)
    val trainedNetwork = network.learn(DenseMatrix(delimited.data: _*), delimited.getNumericTargets.toArray)
    val errorList = (delimitedTest.data zip delimitedTest.getNumericTargets).map {
      case (arr, actualOutput) => trainedNetwork.predict(arr) == actualOutput
    }
    val (nonErrors, errors) = errorList.partition(isError => isError)

    println(nonErrors.length + " " + errors.length)
    assert(nonErrors.length == 1985)
    assert(errors.length == 22)
  }

  it should "be able to apply separate files validation with LMS USPS" in {
    pending //TODO:fix me
    val zipTrainingPath = getClass.getResource("/zip.train").getPath
    val zipTestPath = getClass.getResource("/zip.test").getPath
    val delimitedParser = new DelimitedParser(0)
    val delimited = delimitedParser.parse(zipTrainingPath)
    val delimitedTest = delimitedParser.parse(zipTestPath)
    val inputNodesNum = delimited.data.head.length
    val network = Network(LeastMeanSquares, LogisticSigmoid, inputNodesNum, 40, delimited.getNumericTargets.max + 1)
    val trainedNetwork = network.learn(DenseMatrix(delimited.data: _*), delimited.getNumericTargets.toArray)
    val errorList = (delimitedTest.data zip delimitedTest.getNumericTargets).map {
      case (arr, actualOutput) => trainedNetwork.predict(arr) == actualOutput
    }
    val (nonErrors, errors) = errorList.partition(isError => isError)

    println(nonErrors.length + " " + errors.length)
    assert(nonErrors.length == 1985)
    assert(errors.length == 22)
  }


}
