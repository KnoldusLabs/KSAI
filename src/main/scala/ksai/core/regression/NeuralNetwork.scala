package ksai.core.regression

import breeze.linalg.{DenseMatrix, DenseVector}
import ksai.util.NumericFunctions

import breeze.linalg._
import breeze.numerics._

import scala.util.Try

/**
  * Neural network for Regression
  */

sealed trait ActivationFunction

case object TANH extends ActivationFunction

case object LogisticSigmoid extends ActivationFunction

case class Layer(
                  units: Int,
                  output: DenseVector[Double],
                  error: DenseVector[Double],
                  weight: DenseMatrix[Double],
                  delta: DenseMatrix[Double]
                )

case class Network(
                    dimension: Int, //Number of feature
                    numOfClass: Int,
                    net: Seq[Layer],
                    numUnits: Seq[Int],
                    activationFunction: ActivationFunction,
                    learningRate: Double = 0.1,
                    momentum: Double = 0.0,
                    weightDecay: Double = 0.0 //factor. Regularization term
                  ) {
  require(numUnits.lengthCompare(2) >= 0, s"Invalid number of layers: ${numUnits.length}")
  require(!numUnits.exists(_ < 1), "numUnits cannot contain less than 1 value")

  //labels has to fix here
  def learn(features: DenseMatrix[Double], targets: Array[Double]): Network = {
//    val target: Array[Double] = Array.fill[Double](this.numUnits.last)(0.0)
    features(*, ::).map{x => Some(x)}.toArray.flatten.zipWithIndex.foldLeft(this) {
      case (network, (featureRow, index)) => learn(featureRow, targets(index), network)
    }
  }

  def propagate(lower: Layer, upper: Layer, isUpperAOutputLayer: Boolean, network: Network): Layer = {
    val summedWeights = upper.weight * lower.output
    if(isUpperAOutputLayer){
      upper.copy(output = summedWeights)
    } else {
      network.activationFunction match {
        case LogisticSigmoid =>
          upper.copy(output = summedWeights.map(NumericFunctions.logisticSigmoid))
        case TANH =>
          upper.copy(output = summedWeights.map(weigh => (2 * NumericFunctions.logisticSigmoid(2 * weigh)) - 1))
        case _ => throw new UnsupportedOperationException("Unsupported activation function.")
      }
    }

  }

  private def propagate(network: Network): List[Layer] = {
    val exceptTheOutputLayer = network.net.dropRight(1)

    val propagatedLayers: List[Layer] = exceptTheOutputLayer.tail.foldLeft(List(exceptTheOutputLayer.head)){
      case (resultLayer, upperLayer) => resultLayer :+ propagate(resultLayer.last, upperLayer, false, network)
    }
    val newOutputLayer = propagate(propagatedLayers.last, network.net.last, true, network)

    propagatedLayers :+ newOutputLayer
  }


  def computeOutputError(output: Double, outputLayer: Layer): (Double, Double) = {
    val gradient = output - outputLayer.output(0)
    val error = 0.5 * gradient * gradient
    (error, gradient)
  }

  def learn(features: DenseVector[Double], target: Double, network: Network, weight: Double = 1.0): Network = {
    val inputLayer = network.net.head.copy(output = features)

    val newLayersAfterPropagation: List[Layer] = propagate(network)
    val (error, outputLayerError) = computeOutputError(target, network.net.last)

    val weightedOutputLayerErrors = if (weight != 1.0) {
      outputLayerError * weight
    } else {
      outputLayerError
    }

    val outputLayerAfterErrorCalculation = newLayersAfterPropagation.last.copy(error = DenseVector(Array(weightedOutputLayerErrors)))
    val newLayers = List(inputLayer) ::: newLayersAfterPropagation.dropRight(1) ::: List(outputLayerAfterErrorCalculation)

    val errWeight = weight * error

    val newNetwork = network.copy(net = newLayers)

    val backPropagatedNetwork = backpropagate(newNetwork)
    val weightAdjustedNetwork = adjustWeights(backPropagatedNetwork)

    weightAdjustedNetwork
  }

  private def backpropagate(upper: Layer, lower: Layer): Layer = {
    val lowerErrors = (0 until lower.units).map {
      case lowerIndex =>
        val upperError: Double = (0 until upper.units).map {
          case upperIndex =>
            upper.weight(upperIndex, lowerIndex) * upper.error(upperIndex)
        }.sum
        activationFunction match {
          case LogisticSigmoid =>
            lower.output(lowerIndex) * (1.0 - lower.output(lowerIndex)) * upperError
          case TANH =>
            (1.0 - (lower.output(lowerIndex) * lower.output(lowerIndex))) * upperError
          case _ => (1.0 - (lower.output(lowerIndex) * lower.output(lowerIndex))) * upperError
        }

    }
    lower.copy(error = DenseVector(lowerErrors.toArray))
  }

  private def backpropagate(network: Network): Network = {
   val newNetworkLayers = network.net.dropRight(1).tail.foldRight(List(network.net.last)){
      case (layer, result) =>
       val newLowerLayer = backpropagate(result.head, layer)
        List(newLowerLayer) ::: result
    }
    network.copy(net = newNetworkLayers)
  }

  private def adjustWeights(network: Network) = {
    val adjustedWeightedLayers = network.net.sliding(2).map{
      case List(layer1, layer2) =>
        layer2.error.data.zipWithIndex.map{
          case (error, layer2Index) => layer1.output.toArray.zipWithIndex.map{
            case (layer2Output, layer1index) =>
              val delta = (1 - momentum) * learningRate * error * layer2Output + momentum * layer2.delta(layer2Index, layer1index)
              layer2.delta(layer2Index, layer1index) = delta
              layer2.weight(layer2Index, layer1index) = layer2.weight(layer2Index, layer1index) + delta
              if(weightDecay != 0.0 && layer1index < layer1.units){
                layer2.weight(layer2Index, layer1index) = layer2.weight(layer2Index, layer1index) - learningRate * weightDecay
              }
          }
        }
        layer2
    }.toList
    val newLayers = network.net.head +: adjustedWeightedLayers
    network.copy(net = newLayers)
  }

  def setInput(x: Array[Double]) = {
    if (x.length != this.net.head.units) {
      throw new IllegalArgumentException(s"Invalid input vector size: ${x.length}, expected: ${this.net.head.units}")
    }
    val newInputLayer = this.net.head.copy(output = DenseVector(x))
    val newNet = newInputLayer +: this.net.tail
    this.copy(net = newNet)
  }

  def predict(x: Array[Double]): Double = {
    val newNetwork = setInput(x)
    propagate(newNetwork)

    this.net.last.output(0)
  }

}

object Network {
  def apply(numUnits: Int*): Network = {
    commonApply(numUnits, LogisticSigmoid)
  }

  def apply(activationFunction: ActivationFunction, numUnits: Int*): Network = {
    commonApply(numUnits, activationFunction)
  }


  private def commonApply(numUnits: Seq[Int], activationFunction: ActivationFunction): Network = {
    val finalNumClass = if (numUnits.last == 1) {
      2
    } else {
      numUnits.last
    }

    val net = numUnits.zipWithIndex.map {
      case (numUnit, index) =>
        if(index == 0){
          new DenseMatrix(0, 0, Array.empty[Double])
          Layer(numUnit, DenseVector.zeros(numUnit), DenseVector.zeros(numUnit),
            new DenseMatrix(0, 0, Array.empty[Double]),
            new DenseMatrix(0, 0, Array.empty[Double]))
        } else {
          Layer(numUnit, DenseVector.zeros(numUnit), DenseVector.zeros(numUnit),
            DenseMatrix.rand[Double](numUnit, numUnits(index - 1)),
            DenseMatrix.zeros[Double](numUnit, numUnits(index - 1)))
        }
    }
    new Network(dimension = numUnits.head, numOfClass = finalNumClass, net = net, numUnits = numUnits, activationFunction)
  }

}

case class Trainer(
                    numUnits: Seq[Int],
                    activationFunction: ActivationFunction = LogisticSigmoid,
                    learningRate: Double = 0.1,
                    momentum: Double = 0.0,
                    weightDecay: Double = 0.0,
                    epochs: Int = 25 //tocastic learning
                  ) {

  require((numUnits lengthCompare 2) >= 0, s"Invalid number of layers: ${numUnits.length}")
  require(!numUnits.exists(_ < 1), "numUnits cannot contain less than 1 value")
  require(learningRate > 0, "Learning Rate cannot be 0 or less")
  require(momentum > 0.0 && momentum < 1.0, "Momentum has to be between 0 and 1")
  require(weightDecay > 0.0 && weightDecay < 0.1, "WeightDecay has to be between 0 and .1")
  require(epochs > 1, "Epoch must be greater than 1")


  /*@Override
  public NeuralNetwork train(double[][] x, double[] y) {
    NeuralNetwork net = new NeuralNetwork(activationFunction, numUnits);
    net.setLearningRate(eta);
    net.setMomentum(alpha);
    net.setWeightDecay(lambda);

    for (int i = 1; i <= epochs; i++) {
      net.learn(x, y);
      logger.info("Neural network learns epoch {}", i);
    }

    return net;
  }*/



}

case object Trainer {

  def apply(numUnits: Int*) = {
    new Trainer(numUnits, LogisticSigmoid)
  }

  def apply(activationFunction: ActivationFunction, numUnits: Int*) {
    new Trainer(numUnits, activationFunction)
  }
}

class NeuralNetwork {

}
