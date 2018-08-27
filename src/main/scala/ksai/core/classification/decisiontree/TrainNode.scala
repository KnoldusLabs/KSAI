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
        constructSampleForNumeric(0, new Array[Int](trainingInstances.length), 0, 0)

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
}

object TrainNode {

  def apply(node: Node,
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            samples: Array[Int]
           ): TrainNode = new TrainNode(node, trainingInstances, labels, samples)
}
