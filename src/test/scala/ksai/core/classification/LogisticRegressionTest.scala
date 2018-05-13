package ksai.core.classification

import org.scalatest.{FlatSpec, Matchers}

class LogisticRegressionTest extends FlatSpec with Matchers {

  it should " test the minimization function" in {
    val func = new DifferentiableMultivariateFunction {
      override def f(x: Array[Double], gradient: Array[Double]): Double = {
        val ans = (1 to x.length).filter(_ % 2 != 0).foldLeft(0.0){(sum, j) =>
          val t1 = 1E0 - x(j - 1)
          val t2 = 1E1 * (x(j) - x(j - 1) * x(j - 1))
          gradient(j + 1 - 1) = 2E1 * t2
          gradient(j - 1) = -2E0 * (x(j - 1) * gradient(j - 1 + 1) + t1)
          sum + t1 * t1 + t2 * t2
        }
        ans
      }

      override def f(x: Array[Double]): Double = {
        (1 to x.length).filter(_ % 2 != 0).foldLeft(0.0){(sum, j) =>
          val t1 = 1E0 - x(j -1)
          val t2 = 1E1 * (x(j) - x(j - 1) * x(j - 1))
          sum + t1 * t1 + t2 * t2
        }
      }
    }

    val x = new Array[Double](100)
    for (j <- 1 to x.length by 2) {
      x(j - 1) = -1.2E0
      x(j + 1 - 1) = 1E0
    }

    val result = LogisticRegression.min(func, 5, x, 0.0001, 200)
    result shouldEqual 3.2760183604E-14 +- 1E-15
  }

}
