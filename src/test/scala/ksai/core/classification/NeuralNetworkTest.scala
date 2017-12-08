package ksai.core.classification

import breeze.linalg.{DenseMatrix, DenseVector}
import ksai.data.parser.ARFFParser
import org.scalatest.{FlatSpec, Matchers}



class NeuralNetworkTest extends FlatSpec with Matchers{


  "A Network" should "select a Activation functional automatically for given error function" in {
    pending
    val net = Network(LeastMeanSquares, 5,4,3,2)
    net.activationFunction should be theSameInstanceAs(LogisticSigmoid)
  }

  it should "generate random values for weight" in {
    pending
    val net = Network(LeastMeanSquares, 5,4,3,2)
    println(net.net.last.weight)
    println()
    println(net.net.last.delta)
  }

  it should "Create a network when passed densematrix and its a temoporary test case" in {
    val arffFile = getClass.getResource("/iris.arff").getPath
    val arff = ARFFParser.parse(arffFile)
    val inputNodesNum = arff.data.head.size
    val network = Network(CrossEntropy, SoftMax, inputNodesNum, 10, 3)
    val trainedNetwork = network.learn(DenseMatrix(arff.data:_*), arff.getNumericTargets.toArray)
    println(trainedNetwork)
    assert(trainedNetwork.learningRate != 0)
  }


}
