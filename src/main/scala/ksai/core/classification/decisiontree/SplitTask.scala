package ksai.core.classification.decisiontree

import akka.actor.{Actor, Props}
import ksai.core.classification.{NOMINAL, NUMERIC, NominalAttribute}

import scala.annotation.tailrec

case class BestSplit(n: Int,
                     count: Array[Int],
                     falseCount: Array[Int],
                     impurity: Double,
                     j: Int,
                     decisionTree: DecisionTree)

class SplitTask(trainingInstances: Array[Array[Double]],
                labels: Array[Int],
                samples: Array[Int]) extends Actor {

  override def receive = {
    case BestSplit(n, count, falseCount, impurity, j, decisionTree) =>
      val currentSender = sender()
      currentSender ! findBestSplit(n, count, falseCount, impurity, j, decisionTree)
  }

  private def findBestSplit(n: Int,
                    count: Array[Int],
                    falseCount: Array[Int],
                    impurity: Double,
                    j: Int,
                    decisionTree: DecisionTree): Node = {
    val splitNode = Node()

    decisionTree.attributes(j).`type` match {
      case NOMINAL =>
        val m = decisionTree.attributes(j).asInstanceOf[NominalAttribute].size()
        val trueCount = Array.ofDim[Int](m, decisionTree.noOfClasses)

        trainingInstances.zipWithIndex.foreach { case (trainingInstance, index) =>
          if (samples(index) > 0) {
            trueCount(trainingInstance(j).toInt)(labels(index)) += samples(index)
          }
        }

        getSplitNodeForNominal(0, m, count, impurity, trueCount, n, decisionTree, splitNode, j)

      case NUMERIC =>
        decisionTree.order(j).fold(splitNode) { orderArray =>
          getSplitNodeForNumeric(splitNode, orderArray, j, n, count, decisionTree, impurity)
        }

      case attributeType => throw new IllegalStateException("Unsupported Attribute type: " + attributeType)
    }
  }

  @tailrec
  private final def getSplitNodeForNominal(currentM: Int,
                                           m: Int,
                                           count: Array[Int],
                                           impurity: Double,
                                           trueCount: Array[Array[Int]],
                                           n: Int,
                                           decisionTree: DecisionTree,
                                           splitNode: Node,
                                           j: Int): Node = {
    if (currentM >= m) {
      splitNode
    } else {
      val tc = trueCount(currentM).sum
      val fc = n - tc

      if (tc < decisionTree.nodeSize || fc < decisionTree.nodeSize) {
        getSplitNodeForNominal(currentM + 1, m, count, impurity, trueCount, n, decisionTree, splitNode, j)
      } else {

        val falseCount = (0 to decisionTree.noOfClasses).map(q => count(q) - trueCount(currentM)(q)).toArray

        val trueLabel = trueCount(currentM).indexOf(trueCount(currentM).max)
        val falseLabel = falseCount.indexOf(falseCount.max)

        val gain =
          impurity - tc.toDouble / n * decisionTree.impurity(trueCount(currentM), tc) - fc.toDouble / n * decisionTree.impurity(falseCount, fc)

        if (gain > splitNode.splitScore) {
          Node(splitFeature = j,
            splitValue = currentM,
            splitScore = gain,
            trueChildOutput = trueLabel,
            falseChildOutput = falseLabel)
        } else {
          splitNode
        }
      }
    }
  }

  private final def getSplitNodeForNumeric(
                                            splitNode: Node,
                                            orderArray: Array[Int],
                                            j: Int,
                                            n: Int,
                                            count: Array[Int],
                                            decisionTree: DecisionTree,
                                            impurity: Double): Node = {
    val trueCount = new Array[Int](decisionTree.noOfClasses)
    var prevX = Double.NaN
    var prevY = -1
    orderArray.foreach { index =>
      if (samples(index) > 0) {
        if (prevX.isNaN || trainingInstances(index)(j) == prevX || labels(index) == prevY) {
          prevX = trainingInstances(index)(j)
          prevY = labels(index)
          trueCount(labels(index)) += samples(index)
        } else {

          val tc = trueCount.sum
          val fc = n - tc

          if (tc < decisionTree.nodeSize || fc < decisionTree.nodeSize) {
            prevX = trainingInstances(index)(j)
            prevY = labels(index)
            trueCount(labels(index)) += samples(index)
          } else {
            val falseCount = (0 until decisionTree.noOfClasses).map(q => count(q) - trueCount(q)).toArray

            val trueLabel = trueCount.indexOf(trueCount.max)
            val falseLabel = falseCount.indexOf(falseCount.max)

            val gain =
              impurity - tc.toDouble / n * decisionTree.impurity(trueCount, tc) - fc.toDouble / n * decisionTree.impurity(falseCount, fc)

            if (gain > splitNode.splitScore) {
              splitNode.splitFeature = j
              splitNode.splitValue = (trainingInstances(index)(j) + prevX) / 2
              splitNode.splitScore = gain
              splitNode.trueChildOutput = trueLabel
              splitNode.falseChildOutput = falseLabel
            }

            prevX = trainingInstances(index)(j)
            prevY = labels(index)
            trueCount(labels(index)) += samples(index)
          }
        }
      }
    }

    splitNode
  }
}

object SplitTask {

  def props(trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            samples: Array[Int]) = Props(new SplitTask(trainingInstances, labels, samples))
}
