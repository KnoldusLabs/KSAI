package ksai.core.classification

import breeze.linalg.{DenseMatrix, DenseVector}
import ksai.util.NumericFunctions

import breeze.linalg._
import breeze.numerics._

import scala.util.Try

sealed trait ErrorFunction

case object LeastMeanSquares extends ErrorFunction

case object CrossEntropy extends ErrorFunction

sealed trait ActivationFunction

case object Linear extends ActivationFunction

case object LogisticSigmoid extends ActivationFunction

case object SoftMax extends ActivationFunction

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
                    errorFunction: ErrorFunction,
                    activationFunction: ActivationFunction,
                    learningRate: Double = 0.1,
                    momentum: Double = 0.0,
                    weightDecay: Double = 0.0 //factor. Regularization term
                    //                    target: DenseVector[Double]
                  ) {
  require(numUnits.length >= 2, s"Invalid number of layers: ${numUnits.length}")
  require(numUnits.filter(_ < 1).isEmpty, "numUnits cannot contain less than 1 value")
  require(validateErrorAndActivationFunc == "perfect", validateErrorAndActivationFunc)

  private def validateErrorAndActivationFunc: String = errorFunction match {
    case LeastMeanSquares =>
      activationFunction match {
        case SoftMax => "Sofmax activation function is invalid for least mean squares error."
        case _ => "perfect"
      }
    case CrossEntropy =>
      activationFunction match {
        case Linear => "Linear activation function is invalid with cross entropy error."
        case SoftMax if numUnits.last == 1 => "Softmax activation function is for multi-class."
        case LogisticSigmoid if numUnits.last != 1 => "For cross entropy error, logistic sigmoid output is for binary classification."
        case _ => "perfect"
      }
  }

  //labels has to fix here
  //TODO: pass Network here to learn well, in smile the network is being used as reference.
  def learn(features: DenseMatrix[Double], labels: Array[Int]): Network = {
    val target: Array[Double] = Array.fill[Double](this.numUnits.last)(0.0)
    features(*, ::).map{x => Some(x)}.toArray.flatten.zipWithIndex.foldLeft(this) {
      case (network, (featureRow, index)) => learn(featureRow, labels(index), target, network)
    }
  }

  private def learn(feature: DenseVector[Double], label: Int,
                    target: Array[Double], network: Network, weight: Double = 1.0): Network = {
    if (weight < 0.0) throw new IllegalArgumentException("Invalid weight: " + weight)

    if (weight == 0.0) throw new Exception("Ignore the training instance with zero weight.")

    if (label < 0) throw new IllegalArgumentException("Invalid class label: " + label)

    //net.last is the output layer
    if (network.net.last.units == 1 && label > 1) throw new IllegalArgumentException("Invalid class label: " + label)

    if (network.net.last.units > 1 && label >= network.net.last.units) throw new IllegalArgumentException("Invalid class label: " + label)

    val targetReinitialized = network.errorFunction match {
      case CrossEntropy =>
        network.activationFunction match {
          case LogisticSigmoid =>
            if (label == 0) {
              target.update(0, 1.0)
              target
            } else {
              target.update(0, 0.0)
              target
            }
          case _ =>
            val allZeroArr = Array.fill[Double](target.length)(0.0)
            allZeroArr.update(label, 1.0)
            allZeroArr
        }

      case _ =>
        val allZeroArr = Array.fill[Double](target.length)(0.1)
        allZeroArr.update(label, 0.9)
        allZeroArr
    }
    learn(feature, targetReinitialized, weight, network)
  }

  def propagate(lower: Layer, upper: Layer, isUpperAOutputLayer: Boolean, network: Network): Layer = {
    val summedWeights = upper.weight * lower.output
    network.activationFunction match {
      case LogisticSigmoid if !isUpperAOutputLayer =>
        upper.copy(output = summedWeights.map(NumericFunctions.logisticSigmoid))
      case LogisticSigmoid if isUpperAOutputLayer =>
        softmax(upper.copy(output = summedWeights))
      case _ =>
        network.activationFunction match {
          case Linear | SoftMax => upper.copy(output = summedWeights)
        }
      case _ => throw new UnsupportedOperationException("Unsupported activation function.")
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


  def softmax(outputLayer: Layer): Layer = {
    val outputMax = outputLayer.output.max
    val max = if (outputLayer.output.max > Double.NegativeInfinity) outputMax else Double.NegativeInfinity
    val maxExpOutput = outputLayer.output.map(value => Math.exp(value - max))
    val maxExpSum = sum(maxExpOutput)
    val softMaxedOutput = maxExpOutput.map(_ / maxExpSum)
    outputLayer.copy(output = softMaxedOutput)
  }


  /*private double computeOutputError(double[] output) {
    return computeOutputError(output, outputLayer.error);
  }*/


  def computeOutputError(target: Array[Double], outputLayer: Layer, network: Network): (Double, Array[Double]) = {
    if (target.length != outputLayer.units) {
      throw new IllegalArgumentException(String.format(s"Invalid output vector size: ${target.length}, expected: ${outputLayer.units}"))
    }
    val gradients = (outputLayer.output.toArray zip target map {
      case (outputValue, targetValue) => outputValue - targetValue
    })
    val error = network.errorFunction match {
      case LeastMeanSquares =>
        gradients.foldLeft(0.0) {
          case (result, g) => result + (0.5 * g * g)
        }
      case CrossEntropy =>
        network.activationFunction match {
          case SoftMax =>
            (outputLayer.output.toArray zip target map {
              case (outputValue, targetValue) => targetValue * NumericFunctions.log(outputValue)
            }).foldLeft(0.0) {
              case (result, g) => result - g
            }
          case LogisticSigmoid =>
            (outputLayer.output.toArray zip target map {
              case (outputValue, targetValue) =>
                -targetValue * NumericFunctions.log(outputValue) - (1.0 - targetValue) * NumericFunctions.log(1.0 - outputValue)
            }).foldLeft(0.0) {
              case (_, g) => g
            }
        }
    }

    val finalGradients = (network.errorFunction, network.activationFunction) match {
      case (LeastMeanSquares, LogisticSigmoid) => outputLayer.output.toArray zip gradients map {
        case (outputValue, gradient) => gradient * (1.0 - outputValue)
      }
      case _ => gradients
    }

    (error, finalGradients)
  }

  def learn(features: DenseVector[Double], target: Array[Double], weight: Double, network: Network): Network = {
    val inputLayer = network.net.head.copy(output = features)

    val newLayersAfterPropagation: List[Layer] = propagate(network)
    val (error, outputLayerErrors) = computeOutputError(target, network.net.last, network)

    val weightedOutputLayerErrors = if (weight != 1.0) {
      outputLayerErrors.map(_ * weight)
    } else {
      outputLayerErrors
    }

    val outputLayerAfterErrorCalculation = newLayersAfterPropagation.last.copy(error = DenseVector(weightedOutputLayerErrors))
    val newLayers = List(inputLayer) ::: newLayersAfterPropagation.dropRight(1) ::: List(outputLayerAfterErrorCalculation)

    val errWeight = weight * error

    val newNetwork = network.copy(net = newLayers)

    val backPropagatedNetwork = backpropagate(newNetwork)
    val weightAdjustedNetwork = adjustWeights(backPropagatedNetwork)

    weightAdjustedNetwork
  }

  private def backpropagate(upper: Layer, lower: Layer): Layer = {
    val lowerErrors = (0 to (lower.units-1)).map {
      case lowerIndex =>
        val upperError: Double = (0 to (upper.units-1)).map {
          case upperIndex =>
            upper.weight(upperIndex, lowerIndex) * upper.error(upperIndex)
        }.sum
        lower.output(lowerIndex) * (1.0 - lower.output(lowerIndex)) * upperError
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

  def predict(x: Array[Double]): Int = {
    val newNetwork = setInput(x)
    propagate(newNetwork)

    if (this.net.last.units == 1) {
      if (this.net.last.output(0) > 0.5) {
        0
      } else {
        1
      }
    } else {
     val (_, result) =  this.net.last.output.toArray.zipWithIndex.foldLeft((Double.NegativeInfinity, 0)){
        case ((max, result), (output, index)) => if(output > max) (output, index) else (max, result)
      }
      result
    }
  }

}

object Network {
  def apply(errorFunction: ErrorFunction, numUnits: Int*): Network = {
    commonApply(errorFunction, getNaturalAF(errorFunction, numUnits.last), numUnits)
  }

  def apply(errorFunction: ErrorFunction, activationFunction: ActivationFunction, numUnits: Int*): Network = {
    commonApply(errorFunction, activationFunction, numUnits)
  }


  private def commonApply(errorFunction: ErrorFunction, activationFunction: ActivationFunction, numUnits: Seq[Int]): Network = {
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
    new Network(dimension = numUnits.head, numOfClass = finalNumClass, net = net, numUnits = numUnits, errorFunction, activationFunction)
  }

  private def getNaturalAF(errorFunction: ErrorFunction, numOutputNode: Int): ActivationFunction = {
    errorFunction match {
      case CrossEntropy => if (numOutputNode == 1) {
        LogisticSigmoid
      } else {
        SoftMax
      }
      case LeastMeanSquares => LogisticSigmoid
    }
  }

}

case class Trainer(
                    errorFunction: ErrorFunction,
                    activationFunction: ActivationFunction,
                    numUnits: Seq[Int],
                    learningRate: Double = 0.1,
                    momentum: Double = 0.0,
                    weightDecay: Double = 0.0,
                    epochs: Int = 25 //tocastic learning
                  ) {

  require(numUnits.length >= 2, s"Invalid number of layers: ${numUnits.length}")
  require(numUnits.filter(_ < 1).isEmpty, "numUnits cannot contain less than 1 value")
  require(validateErrorAndActivationFunc == "perfect", validateErrorAndActivationFunc)

  private def validateErrorAndActivationFunc: String = errorFunction match {
    case LeastMeanSquares =>
      activationFunction match {
        case SoftMax => "Sofmax activation function is invalid for least mean squares error."
        case _ => "perfect"
      }
    case CrossEntropy =>
      activationFunction match {
        case Linear => "Linear activation function is invalid with cross entropy error."
        case SoftMax if numUnits.last == 1 => "Softmax activation function is for multi-class."
        case LogisticSigmoid if numUnits.last != 1 => "For cross entropy error, logistic sigmoid output is for binary classification."
        case _ => "perfect"
      }
  }

  /*def train(features: DenseMatrix[Double], labels: Array[Int], target: Array[Double]): Network = {
    val network = Network(errorFunction, activationFunction, numUnits: _*)
    (1 to epochs).foldLeft(network) {
      case (result, _) => result.learn(features, labels, target)
    }
  }*/

}

case object Trainer {

  def apply(errorFunction: ErrorFunction, numUnits: Int*) = {
    new Trainer(errorFunction, getNaturalAF(errorFunction, numUnits.last), numUnits)
  }

  def apply(errorFunction: ErrorFunction, activationFunction: ActivationFunction, numUnits: Int*) {
    new Trainer(errorFunction, activationFunction, numUnits)
  }

  private def getNaturalAF(errorFunction: ErrorFunction, numOutputNode: Int) = {
    errorFunction match {
      case CrossEntropy => if (numOutputNode == 1) {
        LogisticSigmoid
      } else {
        SoftMax
      }
      case LeastMeanSquares => LogisticSigmoid
    }
  }

}

class NeuralNetwork {

}
