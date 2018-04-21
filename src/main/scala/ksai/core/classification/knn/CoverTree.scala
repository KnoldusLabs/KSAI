package ksai.core.classification.knn

trait Metric

case class CoverTree(
                    data: List[Double],
                    distance: Metric,
                    root: Node,
                    base: Double = 1.3,

                    ){

  val invLogBase: Double = 1.0 / Math.log(base)

  val identicalExcluded = true


}
