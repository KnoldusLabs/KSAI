package ksai.core.classification.decisiontree

import ksai.core.classification.decisiontree

/**
  * The criterion to choose variable to split instances.
  */
object SplitRule extends Enumeration {
  type SplitRule = Value

  /**
    * Used by the CART algorithm, Gini impurity is a measure of how often
    * a randomly chosen element from the set would be incorrectly labeled
    * if it were randomly labeled according to the distribution of labels
    * in the subset. Gini impurity can be computed by summing the
    * probability of each item being chosen times the probability
    * of a mistake in categorizing that item. It reaches its minimum
    * (zero) when all cases in the node fall into a single target category.
    */
  val GINI: decisiontree.SplitRule.Value = Value

  /**
    * Used by the ID3, C4.5 and C5.0 tree generation algorithms.
    */
  val ENTROPY: decisiontree.SplitRule.Value = Value

  /**
    * Classification error.
    */
  val CLASSIFICATION_ERROR: decisiontree.SplitRule.Value = Value
}
