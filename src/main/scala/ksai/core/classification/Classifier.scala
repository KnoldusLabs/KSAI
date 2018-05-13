package ksai.core.classification

trait Classifier[T] {

  def predict(x: T): Int

}
