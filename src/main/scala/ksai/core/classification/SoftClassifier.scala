package ksai.core.classification

/**
  * Soft classifiers calculate a posteriori probabilities besides the class
  * label of an instance.
  *
  * @tparam A the type of input object
  */
trait SoftClassifier[A] extends Classifier[A] {


  /**
    * Predicts the class label of an instance and also calculate a posteriori
    * probabilities. Classifiers may NOT support this method since not all
    * classification algorithms are able to calculate such a posteriori
    * probabilities.
    *
    * @param instance   the instance to be classified.
    * @param posteriori posteriori the array to store a posteriori probabilities on output.
    * @return the predicted class label
    */
  def predict(instance: A, posteriori: Array[Double]): Int
}
