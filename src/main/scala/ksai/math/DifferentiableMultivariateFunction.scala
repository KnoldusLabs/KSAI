package ksai.math

trait DifferentiableMultivariateFunction extends MultivariateFunction{
  def f(x: Array[Double], gradient: Array[Double]): Double
}