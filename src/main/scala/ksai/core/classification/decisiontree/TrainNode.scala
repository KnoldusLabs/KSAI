package ksai.core.classification.decisiontree

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.routing.RoundRobinPool
import akka.util.Timeout
import ksai.core.classification.{NOMINAL, NUMERIC}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

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
        /*println("node.splitFeature --> " + node.splitFeature)
        println("node.splitValue --> " + node.splitValue)*/
        constructSampleForNumeric(0, new Array[Int](trainingInstances.length), 0, 0)

      case attributeType =>
        throw new IllegalStateException("Unsupported attribute type: " + attributeType)
    }

    /*println("tc --> " + tc)
    println("fc --> " + fc)*/

    /*println("nodeSize --> " + decisionTree.nodeSize)*/

    if (tc < decisionTree.nodeSize || fc < decisionTree.nodeSize) {
      node.splitFeature = -1
      node.splitValue = Double.NaN
      node.splitScore = 0.0
      false
    } else {
      val (trueChildPosteriori, falseChildPosteriori) =
        constructChildPosterioris(0, trueSamples, new Array[Double](decisionTree.noOfClasses), new Array[Double](decisionTree.noOfClasses))

      (0 until decisionTree.noOfClasses).foreach { index =>
        trueChildPosteriori(index) = (trueChildPosteriori(index) + 1) / (tc + decisionTree.noOfClasses)
        falseChildPosteriori(index) = (falseChildPosteriori(index) + 1) / (fc + decisionTree.noOfClasses)
      }

      /*println("trueChildOutput --> " + node.trueChildOutput)
      println("falseChildOutput --> " + node.falseChildOutput)*/

      node.maybeTrueChild = Option(Node(node.trueChildOutput, Option(trueChildPosteriori)))
      node.maybeFalseChild = Option(Node(node.falseChildOutput, Option(falseChildPosteriori)))

      val falseChild = TrainNode(node.maybeFalseChild.fold(Node())(identity), trainingInstances, labels, samples)

      val trueChild = TrainNode(node.maybeTrueChild.fold(Node())(identity), trainingInstances, labels, trueSamples)

      if (tc > decisionTree.nodeSize && trueChild.findBestSplit(decisionTree)) {
        maybeNextSplits.fold(trueChild.split(None, decisionTree)) { nextSplits =>
          /*println("trueChild splitScore --> " + trueChild.node.splitScore)
          println("trueChild splitValue --> " + trueChild.node.splitValue)
          println("trueChild splitFeature --> " + trueChild.node.splitFeature)
          println("trueChild trueChildOutput --> " + trueChild.node.trueChildOutput)
          println("trueChild falseChildOutput --> " + trueChild.node.falseChildOutput)*/
          nextSplits.add(trueChild)
          false
        }
      }

      if (fc > decisionTree.nodeSize && falseChild.findBestSplit(decisionTree)) {
        maybeNextSplits.fold(falseChild.split(None, decisionTree)) { nextSplits =>
          /*println("falseChild splitScore --> " + falseChild.node.splitScore)
          println("falseChild splitValue --> " + falseChild.node.splitValue)
          println("falseChild splitFeature --> " + falseChild.node.splitFeature)
          println("falseChild trueChildOutput --> " + falseChild.node.trueChildOutput)
          println("falseChild falseChildOutput --> " + falseChild.node.falseChildOutput)*/
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

          node.splitFeature != -1
        }, 10 seconds)
      }
    }
  }

  /*def findBestSplit(n: Int,
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

        val a = getSplitNodeForNominal(0, m, count, impurity, trueCount, n, decisionTree, splitNode, j)
        println("Nominal Split Node Output --> " + a.output)
        a

      case NUMERIC =>
        val trueCount = new Array[Int](decisionTree.noOfClasses)
        val prevX = Double.NaN
        val prevY = -1

        decisionTree.order(j).fold(splitNode) { orderArray =>
          getSplitNodeForNumeric(splitNode, orderArray, j, n, count, decisionTree, impurity)
          //          val a = getSplitNodeForNumeric(splitNode, orderArray, prevX, prevY, j, trueCount, n, count, decisionTree, impurity)
          /*println("splitFeature --> " + a.splitFeature)
          println("splitValue --> " + a.splitValue)
          println("splitScore --> " + a.splitScore)
          println("trueChildOutput --> " + a.trueChildOutput)
          println("falseChildOutput --> " + a.falseChildOutput)*/
        }

      case attributeType => throw new IllegalStateException("Unsupported Attribute type: " + attributeType)
    }
  }*/

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
    /*println("node.splitFeature --> " + node.splitFeature)
    println("node.splitValue --> " + node.splitValue)*/
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

  /*@tailrec
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

  /*@tailrec
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
        if (prevX.isNaN || trainingInstances(index)(j) == prevX || labels(index) == prevY) {
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
            val falseCount = (0 until decisionTree.noOfClasses).map(q => count(q) - trueCount(q)).toArray

            val trueLabel = trueCount.indexOf(trueCount.max)
            val falseLabel = falseCount.indexOf(falseCount.max)

            val gain =
              impurity - tc.toDouble / n * decisionTree.impurity(trueCount, tc) - fc.toDouble / n * decisionTree.impurity(falseCount, fc)
            /*println("trueCount --> " + trueCount.toList)*/

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
        getSplitNodeForNumeric(splitNode,
          orderArray.tail,
          prevX,
          prevY,
          j,
          trueCount,
          n,
          count,
          decisionTree,
          impurity)
      }
    }
  }*/

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
            /*println("trueCount --> " + trueCount.toList)*/

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
  }*/
}

object TrainNode {

  def apply(node: Node,
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            samples: Array[Int]
           ): TrainNode = new TrainNode(node, trainingInstances, labels, samples)
}
