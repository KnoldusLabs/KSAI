package ksai.core.classification.knn


trait Distance{

  //Returns the distance measure between two objects
  def d(x: Array[Double], y: Array[Double]): Double

}

trait Metric extends Distance

case class EuclideanDistance() extends Metric {
  override def d(x: Array[Double], y: Array[Double]): Double = ???
}
