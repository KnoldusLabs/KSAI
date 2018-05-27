package ksai.core.classification

import breeze.linalg.DenseVector

/**
  * Abstract classifier trainer.
  * 
  * [T] the type of input object.
  */
trait ClassifierTrainer[T] {

  def attributes: DenseVector[Attribute]

  def train(x: DenseVector[T], y: DenseVector[Int]): Classifier[T]

}
