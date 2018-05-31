package ksai.core.classification.decisiontree

import ksai.core.classification.Attribute
import ksai.core.classification.decisiontree.SplitRule.SplitRule

class DecisionTree(
                    trainingInstances: Array[Array[Double]],
                    labels: Array[Int],
                    maybeAttributes: Option[Array[Attribute]] = None,
                    splitRule: SplitRule = SplitRule.GINI,
                    nodeSize: Int = 1,
                    maybeSamples: Option[Array[Int]] = None,
                    maybeOrder: Option[Array[Array[Int]]] = None
                  ) {

  val mtry: Int = trainingInstances(0).length
}

object DecisionTree {

  /**
    * Constructor. Learns a classification tree with (most) given number of
    * leaves. All attributes are assumed to be numeric.
    *
    * @param trainingInstances the training instances.
    * @param labels            the response variable.
    * @param maxNodes          the maximum number of leaf nodes in the tree.
    */
  def apply(trainingInstances: Array[Array[Double]], labels: Array[Int], maxNodes: Int) =
    apply(maybeAttributes = None, trainingInstances, labels, maxNodes)

  /**
    * Constructor. Learns a classification tree with (most) given number of
    * leaves.
    *
    * @param maybeAttributes   the attribute properties.
    * @param trainingInstances the training instances.
    * @param labels            the response variable.
    * @param maxNodes          the maximum number of leaf nodes in the tree.
    */
  def apply(maybeAttributes: Option[Array[Attribute]],
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            maxNodes: Int) =
    apply(maybeAttributes, trainingInstances, labels, maxNodes, SplitRule.GINI)

  /**
    * Constructor. Learns a classification tree with (most) given number of
    * leaves.
    *
    * @param maybeAttributes   the attribute properties.
    * @param trainingInstances the training instances.
    * @param labels            the response variable.
    * @param maxNodes          the maximum number of leaf nodes in the tree.
    * @param splitRule         the splitting rule.
    */
  def apply(maybeAttributes: Option[Array[Attribute]],
            trainingInstances: Array[Array[Double]],
            labels: Array[Int],
            maxNodes: Int,
            splitRule: SplitRule) =
    apply(trainingInstances, labels, maxNodes, maybeAttributes, splitRule, trainingInstances(0).length, 1, None, None)

  /**
    * Constructor. Learns a classification tree.
    *
    * @param maybeAttributes   the attribute properties.
    * @param trainingInstances the training instances.
    * @param labels            the response variable.
    * @param nodeSize          the minimum size of leaf nodes.
    * @param maxNodes          the maximum number of leaf nodes in the tree.
    * @param mtry              the number of input variables to pick to split on at each
    *                          node. It seems that sqrt(p) give generally good performance, where p
    *                          is the number of variables.
    * @param splitRule         the splitting rule.
    * @param maybeOrder        the index of training values in ascending order. Note
    *                          that only numeric attributes need be sorted.
    * @param maybeSamples      the sample set of instances for stochastic learning.
    *                          samples[i] is the number of sampling for instance i.
    */
  def apply(
             trainingInstances: Array[Array[Double]],
             labels: Array[Int],
             maxNodes: Int,
             maybeAttributes: Option[Array[Attribute]],
             splitRule: SplitRule,
             nodeSize: Int,
             mtry: Int,
             maybeSamples: Option[Array[Int]],
             maybeOrder: Option[Array[Array[Int]]]
           ): DecisionTree = {
    if (trainingInstances.length != labels.length) {
      throw new IllegalArgumentException(s"The length of training set and labels is not equal. " +
        s"${trainingInstances.length} != ${labels.length}")
    } else if (mtry < 1 || mtry)
  }
}