package ksai.core.classification.randomforest

import ksai.core.classification.{Attribute, NUMERIC}
import ksai.core.classification.decisiontree.SplitRule.SplitRule
import ksai.core.classification.randomforest.RandomForest.Tree

import scala.annotation.tailrec

class RandomForest(trees: List[Tree],
                   error: Double,
                   importance: Array[Double],
                   k: Int = 2) {

}

object RandomForest {

  class Tree {

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
            maybeClassWeight: Option[Array[Int]]): RandomForest = {

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
    }

    new RandomForest(trees, error, importance, k)
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
