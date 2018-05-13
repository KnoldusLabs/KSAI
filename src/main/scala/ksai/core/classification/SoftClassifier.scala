package ksai.core.classification

trait SoftClassifier[T] extends Classifier[T]{
  def predict(instance: T, posterior: Array[Double]): Int
}
