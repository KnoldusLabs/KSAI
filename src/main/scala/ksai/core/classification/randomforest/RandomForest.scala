package ksai.core.classification.randomforest

import akka.actor.{ActorSystem, Props}
import akka.routing.RoundRobinPool
import akka.util.Timeout
import ksai.core.classification.decisiontree.{DecisionTree, SplitTask}
import ksai.core.classification.{Attribute, NUMERIC}
import ksai.core.classification.decisiontree.SplitRule.SplitRule
import ksai.core.classification.randomforest.RandomForest.Tree
import akka.pattern.ask
import scala.concurrent.duration._

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}

class RandomForest(trees: List[Tree],
                   error: Double,
                   importance: Array[Double],
                   k: Int = 2) {

}

object RandomForest {

  case class Tree(tree: DecisionTree, weight: Double)

  def apply(maybeAttributes: Option[Array[Attribute]],
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            ntrees: Int,
            maxNodes: Int,
            nodeSize: Int,
            mtry: Int,
            subsample: Double,
            splitRule: SplitRule,
            maybeClassWeight: Option[Array[Int]])
           (implicit actorSystem: ActorSystem, timeout: Timeout): RandomForest = {

    if (trainingInstances.length != labels.length) {
      throw new IllegalArgumentException(s"The length of training set and labels is not equal. " +
        s"${trainingInstances.length} != ${labels.length}")
    }

    if (mtry < 1 || mtry > trainingInstances(0).length) {
      throw new IllegalArgumentException("Invalid number of variables to split on at a node of the tree: " + mtry)
    }

    if (maxNodes < 2) {
      throw new IllegalArgumentException("Invalid maximum leaves: " + maxNodes)
    }

    if (nodeSize < 1) {
      throw new IllegalAccessException("Invalid minimum size of leaf nodes: " + nodeSize)
    }

    val uniqueLabels = labels.distinct.sorted

    checkForNegativeAndMissingValues(uniqueLabels)

    val noOfClasses = uniqueLabels.length

    if (noOfClasses < 2) {
      throw new IllegalArgumentException("Only one class")
    }

    val attributes = maybeAttributes.fold {
      trainingInstances(0).zipWithIndex.map { case (_, index) =>
        Attribute(`type` = NUMERIC, name = "V" + (index + 1))
      }
    }(identity)

    val classWeight = maybeClassWeight.fold {
      Array.fill[Int](noOfClasses)(1)
    }(identity)

    val n = trainingInstances.length
    val prediction = Array.ofDim[Int](n, uniqueLabels.length)
    val order = attributes.zipWithIndex.map { case (attribute, index) =>
      attribute.`type` match {
        case NUMERIC => //Option(trainingInstances.zipWithIndex.sortBy(_._1(index)).map(_._2))
          //Option(trainingInstances.map(_(index)).zipWithIndex.sortBy(_._1).map(_._2))
          val n = trainingInstances.length
          val a = (0 until n).map { i =>
            trainingInstances(i)(index)
          }.toArray
          Option(a.zipWithIndex.sortBy(_._1).map(_._2))
        case _ => None
      }
    }

    val trainingTask = actorSystem.actorOf(
      RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(Props[TrainingTask])
    )

    val (predictionList, trees) = Await.result(Future.sequence((0 to ntrees).map { _ =>
      (trainingTask ? Train(attributes,
        trainingInstances,
        labels,
        maxNodes,
        nodeSize,
        mtry,
        subsample,
        splitRule,
        classWeight,
        order,
        prediction)).mapTo[PredictionWithWeightedTree]
    }).map { trainList =>
      (trainList.map(_.prediction), trainList.map(_.weightedTree).toArray)
    }, 10 seconds)

    val finalList = predictionList.foldLeft(Array.ofDim[Int](predictionList.length, predictionList(0).length)) { (x, y) =>
      var n = 0
      var m = 0
      while (n < x.length) {
        while (m < x(0).length) {
          x(n)(m) += y(n)(m)
          m += 1
        }
        n += 1
      }

      x
    }

    var m = 0


    new RandomForest(trees, error, importance, noOfClasses)
  }

  private def checkForNegativeAndMissingValues(uniqueLabels: Array[Int]): Boolean = {

    @tailrec
    def iterateListForCheck(list: Array[Int]): Boolean =
      if (list.length > 1) {
        val head = list.head
        val tail = list.tail

        if (head < 0) throw new IllegalArgumentException("Negative class label: " + head)
        if (tail.head - head > 1) throw new IllegalArgumentException("Missing class label: " + (head + 1))

        iterateListForCheck(tail)
      } else {
        true
      }

    iterateListForCheck(uniqueLabels)
  }
}
