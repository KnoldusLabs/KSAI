package ksai.core.classification.randomforest

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.routing.RoundRobinPool
import akka.util.Timeout
import ksai.core.classification.decisiontree.SplitRule.SplitRule
import ksai.core.classification.decisiontree.{DecisionTree, SplitRule}
import ksai.core.classification.randomforest.RandomForest.Tree
import ksai.core.classification.{Attribute, NUMERIC}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

class RandomForest(trees: Array[Tree],
                   noOfClasses: Int = 2) {

  def predict(instance: Array[Double]): Int = {
    val predictions = new Array[Int](noOfClasses)

    trees.foreach { tree =>
      predictions(tree.tree.predict(instance)) += 1
    }

    predictions.indexOf(predictions.max)
  }
}

object RandomForest {

  case class Tree(tree: DecisionTree, weight: Double)

  def apply(trainingInstances: Array[Array[Double]], labels: Array[Int], ntrees: Int)
           (implicit actorSystem: ActorSystem, timeout: Timeout): RandomForest = {
    apply(None, trainingInstances, labels, ntrees)
  }

  def apply(maybeAttributes: Option[Array[Attribute]],
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            ntrees: Int)
           (implicit actorSystem: ActorSystem, timeout: Timeout): RandomForest = {
    apply(maybeAttributes, trainingInstances, labels, ntrees, Math.floor(Math.sqrt(trainingInstances(0).length)).toInt)
  }

  def apply(maybeAttributes: Option[Array[Attribute]],
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            ntrees: Int,
            mtry: Int)
           (implicit actorSystem: ActorSystem, timeout: Timeout): RandomForest = {
    apply(maybeAttributes, trainingInstances, labels, ntrees, 100, 5, mtry, 1.0)
  }

  def apply(maybeAttributes: Option[Array[Attribute]],
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            ntrees: Int,
            maxNodes: Int,
            nodeSize: Int,
            mtry: Int,
            subsample: Double)
           (implicit actorSystem: ActorSystem, timeout: Timeout): RandomForest = {
    apply(maybeAttributes, trainingInstances, labels, ntrees, maxNodes, nodeSize, mtry, subsample, SplitRule.GINI)
  }

  def apply(maybeAttributes: Option[Array[Attribute]],
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            ntrees: Int,
            maxNodes: Int,
            nodeSize: Int,
            mtry: Int,
            subsample: Double,
            splitRule: SplitRule.Value)
           (implicit actorSystem: ActorSystem, timeout: Timeout): RandomForest = {
    apply(maybeAttributes, trainingInstances, labels, ntrees, maxNodes, nodeSize, mtry, subsample, splitRule, None)
  }

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
      trainingInstances(0).indices.map { index =>
        Attribute(`type` = NUMERIC, name = "V" + (index + 1))
      }.toArray
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

    val (predictionList, trees) = Await.result(Future.sequence((0 until ntrees).map { _ =>
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
    }, timeout.duration)

    /*val finalList = predictionList.foldLeft(Array.ofDim[Int](predictionList.length, predictionList(0).length)) { (x, y) =>
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
    }*/

    new RandomForest(trees, noOfClasses)
  }

  private def checkForNegativeAndMissingValues(uniqueLabels: Array[Int]): Unit = {
    uniqueLabels.indices.foreach { i =>
      if (uniqueLabels(i) < 0) {
        throw new IllegalArgumentException("Negative class label: " + uniqueLabels(i))
      }

      if (i > 0 && uniqueLabels(i) - uniqueLabels(i - 1) > 1) {
        throw new IllegalArgumentException("Missing class: " + (uniqueLabels(i) + 1))
      }
    }
  }
}
