package ksai.core.classification.decisiontree

import ksai.core.classification.{NOMINAL, NUMERIC, NominalAttribute}

import scala.annotation.tailrec
import scala.collection.mutable

private[decisiontree] case class TrainNode(node: Node,
                                           trainingInstances: Array[Array[Double]],
                                           labels: Array[Int],
                                           samples: Array[Int]
                                          ) extends Comparable[TrainNode] {

  override def compareTo(trainNode: TrainNode): Int = Math.signum(trainNode.node.splitScore - node.splitScore).toInt

  def split(maybeNextSplits: Option[mutable.PriorityQueue[TrainNode]], decisionTree: DecisionTree): Boolean = {
    if (node.splitFeature < 0) throw new IllegalStateException("Split a node with invalid feature.")

    val (trueSamples, tc, fc) = decisionTree.attributes(node.splitFeature).`type` match {
      case NOMINAL =>
        constructSampleForNominal(0, Array[Int](), 0, 0)

      case NUMERIC =>
        constructSampleForNumeric(0, Array[Int](), 0, 0)

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
        constructChildPosterioris(0, trueSamples, new Array[Double](decisionTree.noOfClasses), new Array[Double](decisionTree.noOfClasses))

      (0 to decisionTree.noOfClasses).foreach { index =>
        trueChildPosteriori(index) = (trueChildPosteriori(index) + 1) / (tc + decisionTree.noOfClasses)
        falseChildPosteriori(index) = (falseChildPosteriori(index) + 1) / (fc + decisionTree.noOfClasses)
      }

      node.maybeTrueChild = Option(Node(node.trueChildOutput, Option(trueChildPosteriori)))
      node.maybeFalseChild = Option(Node(node.falseChildOutput, Option(falseChildPosteriori)))

      val trueChild = TrainNode(node.maybeTrueChild.fold(Node())(identity), trainingInstances, labels, trueSamples)

      if (tc > decisionTree.nodeSize && trueChild.findBestSplit(decisionTree)) {
        maybeNextSplits.fold(trueChild.split(None, decisionTree)) { nextSplits =>
          nextSplits.enqueue(trueChild)
          false
        }
      }

      val falseChild = TrainNode(node.maybeFalseChild.fold(Node())(identity), trainingInstances, labels, samples)

      if (fc > decisionTree.nodeSize && falseChild.findBestSplit(decisionTree)) {
        maybeNextSplits.fold(falseChild.split(None, decisionTree)) { nextSplits =>
          nextSplits.enqueue(falseChild)
          false
        }
      }

      decisionTree.importance(node.splitFeature) += node.splitScore

      true
    }
  }

  def findBestSplit(decisionTree: DecisionTree): Boolean = {
    if (isPure(0, -1)) {
      false
    } else {
      val n = samples.sum
      if (n <= decisionTree.nodeSize) {
        false
      } else {
        println("----------- decision tree classes --> " + decisionTree.noOfClasses)
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

        //TODO: Implement multi-threading
        (0 until decisionTree.mtry).foreach { index =>
          val split = findBestSplit(n, count, new Array[Int](decisionTree.noOfClasses), impurity, variables(index), decisionTree)

          if (split.splitScore > node.splitScore) {
            node.splitFeature = split.splitFeature
            node.splitValue = split.splitValue
            node.splitScore = split.splitScore
            node.trueChildOutput = split.trueChildOutput
            node.falseChildOutput = split.falseChildOutput
          }
        }

        node.splitFeature != -1
      }
    }
  }

  def findBestSplit(n: Int,
                    count: Array[Int],
                    falseCount: Array[Int],
                    impurity: Double,
                    j: Int,
                    decisionTree: DecisionTree): Node = {
    val splitNode = Node()

    decisionTree.attributes(j).`type` match {
      case NOMINAL =>
        val m = decisionTree.attributes(j).asInstanceOf[NominalAttribute].size()
        val trueCount = Array(Array[Int]())

        trainingInstances.zipWithIndex.foreach { case (trainingInstance, index) =>
          if (samples(index) > 0) {
            trueCount(trainingInstance(j).toInt)(labels(index)) += samples(index)
          }
        }

        getSplitNodeForNominal(0, m, count, impurity, trueCount, n, decisionTree, splitNode, j)

      case NUMERIC =>
        val trueCount = new Array[Int](decisionTree.noOfClasses)
        val prevX = Double.NaN
        val prevY = -1

        decisionTree.order(j).fold(splitNode) { orderArray =>
          getSplitNodeForNumeric(splitNode, orderArray, prevX, prevY, j, trueCount, n, count, decisionTree, impurity)
        }

      case attributeType => throw new IllegalStateException("Unsupported Attribute type: " + attributeType)
    }
  }

  @tailrec
  private def isPure(currentIndex: Int, label: Int): Boolean = {
    if (currentIndex >= trainingInstances.length) {
      true
    } else {
      if (samples(currentIndex) > 0) {
        if (label == -1) {
          isPure(currentIndex + 1, labels(currentIndex))
        } else if (labels(currentIndex) != label) {
          false
        } else {
          isPure(currentIndex + 1, label)
        }
      } else {
        isPure(currentIndex + 1, label)
      }
    }
  }

  @tailrec
  private def constructChildPosterioris(currentIndex: Int,
                                        trueSamples: Array[Int],
                                        trueChildPosteriori: Array[Double],
                                        falseChildPosteriori: Array[Double]): (Array[Double], Array[Double]) = {
    if (currentIndex >= trainingInstances.length) {
      (trueChildPosteriori, falseChildPosteriori)
    } else {
      val yi = labels(currentIndex)

      trueChildPosteriori(yi) = trueChildPosteriori(yi) + trueSamples(currentIndex)
      falseChildPosteriori(yi) = falseChildPosteriori(yi) + samples(currentIndex)
      constructChildPosterioris(currentIndex + 1, trueSamples, trueChildPosteriori, falseChildPosteriori)
    }
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

  @tailrec
  private def constructSampleForNumeric(currentIndex: Int, trueSamples: Array[Int], tc: Int, fc: Int): (Array[Int], Int, Int) = {
    if (currentIndex >= trainingInstances.length) {
      (trueSamples, tc, fc)
    } else {
      if (samples(currentIndex) > 0) {
        if (trainingInstances(currentIndex)(node.splitFeature) <= node.splitValue) {
          trueSamples(currentIndex) = samples(currentIndex)
          val newTc = tc + trueSamples(currentIndex)
          samples(currentIndex) = 0
          constructSampleForNumeric(currentIndex + 1, trueSamples, newTc, fc)
        } else {
          val newFc = fc + samples(currentIndex)
          constructSampleForNumeric(currentIndex + 1, trueSamples, tc, newFc)
        }
      } else {
        constructSampleForNumeric(currentIndex + 1, trueSamples, tc, fc)
      }
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

  @tailrec
  private final def getSplitNodeForNumeric(
                                            splitNode: Node,
                                            orderArray: Array[Int],
                                            prevX: Double,
                                            prevY: Int,
                                            j: Int,
                                            trueCount: Array[Int],
                                            n: Int,
                                            count: Array[Int],
                                            decisionTree: DecisionTree,
                                            impurity: Double): Node = {
    if (orderArray.isEmpty) {
      splitNode
    } else {
      val index = orderArray.head
      if (samples(index) > 0) {
        if (prevX != prevX || trainingInstances(index)(j) == prevX || labels(index) == prevY) {
          println("index --> " + index)
          println("j --> " + j)
          println("-------> " + trainingInstances(index)(j))
          trueCount(labels(index)) += samples(index)
          getSplitNodeForNumeric(
            splitNode,
            orderArray.tail,
            trainingInstances(index)(j),
            labels(index),
            j,
            trueCount,
            n,
            count,
            decisionTree,
            impurity)
        } else {

          val tc = trueCount.sum
          val fc = n - tc

          if (tc < decisionTree.nodeSize || fc < decisionTree.nodeSize) {
            trueCount(labels(index)) += samples(index)
            getSplitNodeForNumeric(
              splitNode,
              orderArray.tail,
              trainingInstances(index)(j),
              labels(index),
              j,
              trueCount,
              n,
              count,
              decisionTree,
              impurity)
          } else {

            val falseCount = (0 to decisionTree.noOfClasses).map(q => count(q) - trueCount(q)).toArray

            val trueLabel = trueCount.indexOf(trueCount.max)
            val falseLabel = falseCount.indexOf(falseCount.max)

            val gain =
              impurity - tc.toDouble / n * decisionTree.impurity(trueCount, tc) - fc.toDouble / n * decisionTree.impurity(falseCount, fc)

            val newSplitNode = if (gain > splitNode.splitScore) {
              Node(splitFeature = j,
                splitValue = (trainingInstances(index)(j) + prevX) / 2,
                splitScore = gain,
                trueChildOutput = trueLabel,
                falseChildOutput = falseLabel)
            } else {
              splitNode
            }

            trueCount(labels(index)) += samples(index)
            getSplitNodeForNumeric(
              newSplitNode,
              orderArray.tail,
              trainingInstances(index)(j),
              labels(index),
              j,
              trueCount,
              n,
              count,
              decisionTree,
              impurity)
          }
        }
      } else {
        splitNode
      }
    }
  }
}

object TrainNode {

  def apply(node: Node,
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            samples: Array[Int]
           ): TrainNode = new TrainNode(node, trainingInstances, labels, samples)
}
