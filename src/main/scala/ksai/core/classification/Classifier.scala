package ksai.core.classification


/**
  * A classifier assigns an input object into one of a given number of categories.
  * The input object is formally termed an instance, and the categories are
  * termed classes. The instance is usually described by a vector of features,
  * which together constitute a description of all known characteristics of the
  * instance.
  * <p>
  * Classification normally refers to a supervised procedure, i.e. a procedure
  * that produces an inferred function to predict the output value of new
  * instances based on a training set of pairs consisting of an input object
  * and a desired output value. The inferred function is called a classifier
  * if the output is discrete or a regression function if the output is
  * continuous.
  */


trait Classifier[A] {

  def predict(instance: A): Int

  def predict(instance: Array[A]): Array[Int] = {
    instance.map(element => predict(element))
  }

}
