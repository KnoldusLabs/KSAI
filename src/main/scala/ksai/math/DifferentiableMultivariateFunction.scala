package ksai.math

/**
  * A differentiable function is a function whose derivative exists at each point
  * in its domain.
  */
trait DifferentiableMultivariateFunction extends MultivariateFunction{

  /**
    * Compute the value and gradient of the function at x.
    */
  def f(x: Array[Double], gradient: Array[Double]): Double
}