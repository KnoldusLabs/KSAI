package ksai.core.classification.randomforest

import akka.actor.{Actor, ActorSystem}
import akka.util.Timeout
import ksai.core.classification.Attribute
import ksai.core.classification.decisiontree.DecisionTree
import ksai.core.classification.decisiontree.SplitRule.SplitRule
import scala.concurrent.duration._

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

  implicit val actorSystem = context.system
  implicit val timeout = Timeout(10 seconds)

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
    val k = labels.max + 1
    val samples = new Array[Int](n)
    val tempPrediction = prediction.map(_.clone())

    if (subsample == 1.0) {
      (0 until k).foreach { l =>
        var nj = 0
        val cj = ListBuffer[Int]()
        (0 until n).foreach { i =>
          if (labels(i) == l) {
            cj += i
            nj += 1
          }
        }

        val size = nj / classWeight(l)

        (0 until size).foreach { i =>
          val random = new Random()
          val xi = random.nextInt(nj)
          samples(cj(xi)) += 1
        }
      }
    } else {
      val perm = new Array[Int](n)
      (0 until n).foreach { i =>
        perm(i) = i
      }

      // TODO: PERMUTATION

      val nc = new Array[Int](k)
      (0 until n).foreach(i => nc(labels(i)) += 1)

      (0 until k).foreach { l =>
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

    (0 until n).foreach { i =>
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
