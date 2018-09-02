package ksai.math

/**
  * An interface representing a multivariate real function.
  */
trait MultivariateFunction {

  /**
    * Compute the value of the function at x.
    */
  def f(x: Array[Double]): Double
}
