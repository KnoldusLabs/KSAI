package ksai.core.classification.decisiontree

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.routing.RoundRobinPool
import akka.util.Timeout
import ksai.core.classification.{NOMINAL, NUMERIC, NominalAttribute}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

private[decisiontree] case class TrainNode(node: Node,
                                           trainingInstances: Array[Array[Double]],
                                           labels: Array[Int],
                                           samples: Array[Int]
                                          ) extends Comparable[TrainNode] {
  override def compareTo(trainNode: TrainNode): Int = Math.signum(trainNode.node.splitScore - node.splitScore).toInt

  def split(maybeNextSplits: Option[java.util.PriorityQueue[TrainNode]], decisionTree: DecisionTree)
           (implicit actorSystem: ActorSystem, timeout: Timeout): Boolean = {
    if (node.splitFeature < 0) throw new IllegalStateException("Split a node with invalid feature.")

    val (trueSamples, tc, fc) = decisionTree.attributes(node.splitFeature).`type` match {
      case NOMINAL =>
        constructSampleForNominal(0, new Array[Int](trainingInstances.length), 0, 0)

      case NUMERIC =>
        constructSampleForNumeric

      case attributeType =>
        throw new IllegalStateException("Unsupported attribute type: " + attributeType)
    }

    if (tc < decisionTree.nodeSize || fc < decisionTree.nodeSize) {
      node.splitFeature = -1
      node.splitValue = Double.NaN
      node.splitScore = 0.0
      false
    } else {
      val (trueChildPosteriori, falseChildPosteriori) =
        constructChildPosterioris(0, trueSamples, decisionTree)

      (0 until decisionTree.noOfClasses).foreach { index =>
        trueChildPosteriori(index) = (trueChildPosteriori(index) + 1) / (tc + decisionTree.noOfClasses)
        falseChildPosteriori(index) = (falseChildPosteriori(index) + 1) / (fc + decisionTree.noOfClasses)
      }

      node.maybeTrueChild = Option(Node(node.trueChildOutput, Option(trueChildPosteriori)))
      node.maybeFalseChild = Option(Node(node.falseChildOutput, Option(falseChildPosteriori)))

      val falseChild = TrainNode(node.maybeFalseChild.fold(Node())(identity), trainingInstances, labels, samples)

      val trueChild = TrainNode(node.maybeTrueChild.fold(Node())(identity), trainingInstances, labels, trueSamples)

      if (tc > decisionTree.nodeSize && trueChild.findBestSplit(decisionTree)) {
        maybeNextSplits.fold(trueChild.split(None, decisionTree)) { nextSplits =>
          nextSplits.add(trueChild)
          false
        }
      }

      if (fc > decisionTree.nodeSize && falseChild.findBestSplit(decisionTree)) {
        maybeNextSplits.fold(falseChild.split(None, decisionTree)) { nextSplits =>
          nextSplits.add(falseChild)
          false
        }
      }

      decisionTree.importance(node.splitFeature) += node.splitScore

      true
    }
  }

  def findBestSplit(decisionTree: DecisionTree)(implicit actorSystem: ActorSystem, timeout: Timeout): Boolean = {
    if (isPure(0, -1)) {
      false
    } else {
      val n = samples.sum
      if (n <= decisionTree.nodeSize) {
        false
      } else {
        val count = new Array[Int](decisionTree.noOfClasses)

        trainingInstances.indices.foreach { index =>
          if (samples(index) > 0) {
            count(labels(index)) += samples(index)
          }
        }

        val impurity = decisionTree.impurity(count, n)

        val p = decisionTree.attributes.length
        val variables = new Array[Int](p)

        (0 until p).foreach(index => variables(index) = index)
        val falseCount = new Array[Int](decisionTree.noOfClasses)

        if (decisionTree.mtry < p) {
          permutate(variables)
          var i = 0

          while (i < decisionTree.mtry) {
            val split = findBestSplit(n, count, falseCount, impurity, variables(i), decisionTree)
            if (split.splitScore > node.splitScore) {
              node.splitFeature = split.splitFeature
              node.splitValue = split.splitValue
              node.splitScore = split.splitScore
              node.trueChildOutput = split.trueChildOutput
              node.falseChildOutput = split.falseChildOutput
            }
            i += 1
          }
        } else {

          val splitTask = actorSystem.actorOf(
            RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(SplitTask.props(trainingInstances, labels, samples))
          )

          Await.result(Future.sequence((0 until decisionTree.mtry).map { index =>
            (splitTask ? BestSplit(n, count, falseCount, impurity, variables(index), decisionTree)).mapTo[Node]
          }.toList).map { splitNodes =>
            splitNodes.foreach { split =>
              if (split.splitScore > node.splitScore) {
                node.splitFeature = split.splitFeature
                node.splitValue = split.splitValue
                node.splitScore = split.splitScore
                node.trueChildOutput = split.trueChildOutput
                node.falseChildOutput = split.falseChildOutput
              }
            }
          }, 10 seconds)
        }

        node.splitFeature != -1
      }
    }
  }

  private def isPure(currentIndex: Int, label: Int): Boolean = {
    var mutableLabel = label

    trainingInstances.indices.foreach { i =>
      if (samples(i) > 0) {
        if (mutableLabel == -1) {
          mutableLabel = labels(i)
        } else if (labels(i) != mutableLabel) {
          return false
        }
      }
    }

    true
  }

  private def constructChildPosterioris(currentIndex: Int,
                                        trueSamples: Array[Int],
                                         decisionTree: DecisionTree): (Array[Double], Array[Double]) = {
    val trueChildPosteriori = new Array[Double](decisionTree.noOfClasses)
    val falseChildPosteriori = new Array[Double](decisionTree.noOfClasses)
    trainingInstances.indices.foreach { i =>
      val yi = decisionTree.labels(i)
      trueChildPosteriori(yi) += trueSamples(i)
      falseChildPosteriori(yi) += samples(i)
    }

    (trueChildPosteriori, falseChildPosteriori)
  }

  @tailrec
  private def constructSampleForNominal(currentIndex: Int, trueSamples: Array[Int], tc: Int, fc: Int): (Array[Int], Int, Int) = {
    if (currentIndex >= trainingInstances.length) {
      (trueSamples, tc, fc)
    } else {
      if (samples(currentIndex) > 0) {
        if (trainingInstances(currentIndex)(node.splitFeature) == node.splitValue) {
          trueSamples(currentIndex) = samples(currentIndex)
          val newTc = tc + trueSamples(currentIndex)
          samples(currentIndex) = 0
          constructSampleForNominal(currentIndex + 1, trueSamples, newTc, fc)
        } else {
          val newFc = fc + samples(currentIndex)
          constructSampleForNominal(currentIndex + 1, trueSamples, tc, newFc)
        }
      } else {
        constructSampleForNominal(currentIndex + 1, trueSamples, tc, fc)
      }
    }
  }

  private def constructSampleForNumeric: (Array[Int], Int, Int) = {
    var tc = 0
    var fc = 0
    val trueSamples = new Array[Int](trainingInstances.length)

    trainingInstances.indices.foreach { i =>
      if (samples(i) > 0) {
        if (trainingInstances(i)(node.splitFeature) <= node.splitValue) {
          trueSamples(i) = samples(i)
          tc += trueSamples(i)
          samples(i) = 0
        } else {
          fc += samples(i)
        }
      }
    }

    (trueSamples, tc, fc)
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

  private def permutate(x: Array[Int]): Unit = {
    var i = 0
    val r = Random

    while (i < x.length) {
      val j = i + r.nextInt(x.length - i)
      swap(x, i, j)
      i += 1
    }
  }

  private def swap(x: Array[Int], i: Int, j: Int): Unit = {
    val s = x(i)
    x(i) = x(j)
    x(j) = s
  }

}

object TrainNode {

  def apply(node: Node,
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            samples: Array[Int]
           ): TrainNode = new TrainNode(node, trainingInstances, labels, samples)
}
