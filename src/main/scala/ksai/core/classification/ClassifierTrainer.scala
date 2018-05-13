package ksai.core.classification

import breeze.linalg.DenseVector
import ksai.data.parser.Attribute

trait ClassifierTrainer[T] {

  def attributes: DenseVector[Attribute]

  def train(x: DenseVector[T], y: DenseVector[Int]): Classifier[T]

}
