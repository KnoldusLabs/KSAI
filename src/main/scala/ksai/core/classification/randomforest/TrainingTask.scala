package ksai.core.classification.randomforest

import akka.actor.Actor
import ksai.core.classification.Attribute
import ksai.core.classification.decisiontree.DecisionTree
import ksai.core.classification.decisiontree.SplitRule.SplitRule

import scala.collection.mutable.ListBuffer
import scala.util.Random

case class PredictionWithWeightedTree(prediction: Array[Array[Int]], weightedTree: RandomForest.Tree)

case class Train(attributes: Array[Attribute],
                 trainingInstances: Array[Array[Double]],
                 labels: Array[Int],
                 maxNodes: Int,
                 nodeSize: Int,
                 mtry: Int,
                 subsample: Double,
                 splitRule: SplitRule,
                 classWeight: Array[Int],
                 maybeOrder: Array[Option[Array[Int]]],
                 prediction: Array[Array[Int]])

class TrainingTask extends Actor {

  override def receive = {
    case Train(attributes,
    trainingInstances,
    labels,
    maxNodes,
    nodeSize,
    mtry,
    subsample,
    splitRule,
    classWeight,
    maybeOrder,
    prediction) =>
      val currentSender = sender()
      currentSender ! train(attributes,
        trainingInstances,
        labels,
        maxNodes,
        nodeSize,
        mtry,
        subsample,
        splitRule,
        classWeight,
        maybeOrder,
        prediction)
  }

  private def train(attributes: Array[Attribute],
                    trainingInstances: Array[Array[Double]],
                    labels: Array[Int],
                    maxNodes: Int,
                    nodeSize: Int,
                    mtry: Int,
                    subsample: Double,
                    splitRule: SplitRule,
                    classWeight: Array[Int],
                    maybeOrder: Array[Option[Array[Int]]],
                    prediction: Array[Array[Int]]): PredictionWithWeightedTree = {

    val n = trainingInstances.length
    val k = labels.max
    val samples = new Array[Int](n)
    val tempPrediction = prediction.map(_.clone())

    if (subsample == 1.0) {
      (0 to k).foreach { l =>
        var nj = 0
        val cj = ListBuffer[Int]()
        (0 to n).foreach { i =>
          if (labels(i) == l) {
            cj += i
            nj += 1
          }
        }

        val size = nj / classWeight(l)

        (0 to size).foreach { i =>
          val random = new Random()
          val xi = random.nextInt(nj)
          samples(cj(xi)) += 1
        }
      }
    } else {
      val perm = new Array[Int](n)
      (0 to n).foreach { i =>
        perm(i) = i
      }

      // TODO: PERMUTATION

      val nc = new Array[Int](k)
      (0 to n).foreach(i => nc(labels(i)) += 1)

      (0 to k).foreach { l =>
        val subj = math.round(nc(l) * subsample / classWeight(l)).toInt
        var count = 0
        var i = 0
        while (i < n && count < subj) {
          val xi = perm(i)
          if (labels(xi) == l) {
            samples(xi) += 1
            count += 1
          }
          i += 1
        }
      }
    }

    val dTree =
      DecisionTree(trainingInstances,
        labels,
        maxNodes,
        Option(attributes),
        splitRule,
        nodeSize,
        mtry,
        Option(samples.clone()),
        Option(maybeOrder))

    var oob = 0
    var correct = 0

    (0 to n).foreach { i =>
      if (samples(i) == 0) {
        oob += 1
        val p = dTree.predict(trainingInstances(i))
        if (p == labels(i)) correct += 1
        tempPrediction(i)(p) += 1
      }
    }

    val accuracy = if (oob != 0) correct.toDouble / oob else 1.0

    PredictionWithWeightedTree(tempPrediction, RandomForest.Tree(dTree, accuracy))
  }
}
