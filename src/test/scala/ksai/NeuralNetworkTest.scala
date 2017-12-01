package ksai

import breeze.linalg.{DenseMatrix, DenseVector}
import ksai.core.classification.{LeastMeanSquares, LogisticSigmoid, Network}
import org.scalatest.{FlatSpec, Matchers}



class NeuralNetworkTest extends FlatSpec with Matchers{


  "A Network" should "select a Activation functional automatically for given error function" in {

    val net = Network(LeastMeanSquares, 5,4,3,2)
    net.activationFunction should be theSameInstanceAs(LogisticSigmoid)
  }

  it should "generate random values for weight" in {
    val net = Network(LeastMeanSquares, 5,4,3,2)
    println()
    println(net.net.last.weight)
    println()
    println(net.net.last.delta)
  }


}
