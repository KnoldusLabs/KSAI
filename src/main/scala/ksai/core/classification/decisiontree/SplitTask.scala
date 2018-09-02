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

    var orderIndex = 0
    while (orderIndex < orderArray.length) {
      val index = orderArray(orderIndex)
      if (samples(index) > 0) {
        if (prevX.isNaN || trainingInstances(index)(j) == prevX || labels(index) == prevY) {
          prevX = trainingInstances(index)(j)
          prevY = labels(index)
          trueCount(labels(index)) += samples(index)
        } else {

          var i = 0
          var tc = 0
          while (i < trueCount.length) {
            tc += trueCount(i)
            i += 1
          }

          val fc = n - tc

          if (tc < decisionTree.nodeSize || fc < decisionTree.nodeSize) {
            prevX = trainingInstances(index)(j)
            prevY = labels(index)
            trueCount(labels(index)) += samples(index)
          } else {
            val falseCount = new Array[Int](decisionTree.noOfClasses)
            //            (0 until decisionTree.noOfClasses).foreach(q => falseCount(q) = count(q) - trueCount(q))
            var i = 0
            while (i < decisionTree.noOfClasses) {
              falseCount(i) = count(i) - trueCount(i)
              i += 1
            }

            val trueLabel = getIndexOfMaxElementForArray(trueCount)
            val falseLabel = getIndexOfMaxElementForArray(falseCount)

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
      orderIndex += 1
    }

    splitNode
  }

  def getIndexOfMaxElementForArray(array: Array[Int]): Int = {
    var maxElement = array(0)
    var maxIndex = 0

    var i = 0
    while (i < array.length) {
      if (array(i) > maxElement) {
        maxElement = array(i)
        maxIndex = i
      }
      i += 1
    }

    maxIndex
  }
}

object SplitTask {

  def props(trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            samples: Array[Int]) = Props(new SplitTask(trainingInstances, labels, samples))
}
