package ksai.core.classification

import breeze.linalg._
import breeze.stats.mean

import scala.collection.mutable.ArrayBuffer

/**
  * Created by pranjut on 31/3/18.
  */
case class LDA(
                /**
                  * The dimensionality of data.
                  */
                p: Int,
                /**
                  * The number of classes.
                  */
                k: Int,
                /**
                  * Constant term of discriminant function of each class.
                  */
                ct: ArrayBuffer[Double],
                /**
                  * A priori probabilities of each class.
                  */
                priori: ArrayBuffer[Double],
                /**
                  * Mean vectors of each class.
                  */
                mu: ArrayBuffer[ArrayBuffer[Double]],
                /**
                  * Eigen vectors of common covariance matrix, which transforms observations
                  * to discriminant functions, normalized so that common covariance
                  * matrix is spherical.
                  */
                scaling: DenseMatrix[Double],
                /**
                  * Eigen values of common variance matrix.
                  */
                eigen: DenseVector[Double]
              ) {

  def predict(x: ArrayBuffer[Double], posteriori: ArrayBuffer[Double]): Int = {
    if (x.length != p) {
      throw new IllegalArgumentException(s"Invalid input vector size: ${x.length}, expected: $p")
    }

    if (posteriori != null && posteriori.length != k) {
      throw new IllegalArgumentException(s"Invalid posteriori vector size: ${posteriori.length}, expected: $k")
    }

    var y = 0
    var max = Double.NegativeInfinity

    val d = new ArrayBuffer[Double]()
//    var ux = new ArrayBuffer[Double]()
//    double[] ux = new double[p];

    (0 to k-1).foreach{ index: Int =>
     (0 to p -1).foreach{ jIndex: Int =>
        d += x(jIndex) - mu(index)(jIndex)
      }

//      scaling.atx(d, ux);
      val ux = scaling.t * DenseVector(d:_*)

      var f: Double = 0.0
      (0 to p -1 ).foreach{ jIndex: Int =>
        f += ux(jIndex) * ux(jIndex) / eigen(jIndex)
      }

      f = ct(index) - 0.5 * f
      if (max < f) {
        max = f
        y = index
      }

      if (posteriori != null) {
        posteriori(index) = f
      }
    }

    if (posteriori != null) {
      var sum = 0.0
      (0 to k-1).foreach{ index: Int =>
        posteriori(index) = Math.exp(posteriori(index) - max)
        sum += posteriori(index)
      }

      (0 to k-1).foreach{ index =>
        posteriori(index) /= sum
      }
    }
    y
  }
}

object LDA{

  def apply(x: ArrayBuffer[ArrayBuffer[Double]], y: ArrayBuffer[Int]): LDA = {
    apply(x, y, new ArrayBuffer[Double](), 1E-4)
  }

  def apply(x: ArrayBuffer[ArrayBuffer[Double]], y: ArrayBuffer[Int], priori: ArrayBuffer[Double], tol: Double): LDA = {
    if (x.length != y.length) {
      throw new IllegalArgumentException(s"The sizes of X and Y don't match: ${x.length} != ${y.length}")
    }

    if (!priori.isEmpty) {
      if (priori.length < 2) {
        throw new IllegalArgumentException("Invalid number of priori probabilities: " + priori.length)
      }

      var sum = 0.0
      for (pr: Double <- priori) {
        if (pr <= 0.0 || pr >= 1.0) {
          throw new IllegalArgumentException("Invalid priori probability: " + pr)
        }
        sum += pr
      }

      if (Math.abs(sum - 1.0) > 1E-10) {
        throw new IllegalArgumentException("The sum of priori probabilities is not one: " + sum)
      }
    }

    // class label set.
    var labels: ArrayBuffer[Int] = y.distinct.sortWith{case (first, second) => first < second}

    (0 to labels.length - 1).foreach{ index: Int =>
      if (labels(index) < 0) {
        throw new IllegalArgumentException("Negative class label: " + labels(index))
      }

      if (index > 0 && labels(index) - labels(index - 1) > 1) {
        throw new IllegalArgumentException("Missing class: " + labels(index)+1)
      }
    }

    val newk = labels.length
    if (newk < 2) {
      throw new IllegalArgumentException("Only one class.")
    }

    if (!priori.isEmpty && newk != priori.length) {
      throw new IllegalArgumentException("The number of classes and the number of priori probabilities don't match.")
    }

    if (tol < 0.0) {
      throw new IllegalArgumentException("Invalid tol: " + tol)
    }

    val n = x.length

    if (n <= newk) {
      throw new IllegalArgumentException(s"Sample size is too small: $n <= $newk")
    }

    val newp: Int = x(0).length
    // The number of instances in each class.
    val ni = ArrayBuffer[Int]((0 to newk - 1).map(_ => 0):_*)
    // Common mean vector.
    val denseX: DenseMatrix[Double] = DenseMatrix(x.map(_.toArray):_*)

    val meanX = mean(denseX(*, ::))

    println(s"The means........>>??...${meanX}")
    // Common covariance.
    val C: DenseMatrix[Double] = DenseMatrix.zeros[Double](newp,newp)
    // Class mean vectors.
    val mu = ArrayBuffer[ArrayBuffer[Double]]((0 to newk-1).map(_ => ArrayBuffer((0 to newp-1).map(_ => 0.0):_*)):_*)

    (0 to n-1).foreach{ index: Int =>
      val c = y(index)
      ni += ni(c) + 1
      (0 to newp - 1).foreach{jIndex =>
        mu(c)(jIndex) += x(index)(jIndex)
      }
    }

    (0 to newk-1).foreach{index: Int =>
      (0 to newp-1).foreach{ jIndex: Int =>
        mu(index)(jIndex) /= ni(index)
      }
    }

    if (priori.isEmpty) {
      (0 to newk-1).foreach{ index =>
        priori += ni(index) / n
      }
    }

    val ct = new ArrayBuffer[Double]()
    (0 to newk-1).foreach{index: Int =>
      ct += Math.log(priori(index))
    }

    (0 to n-1).foreach{index: Int =>
      (0 to newp-1).foreach{ jIndex: Int =>
        (0 to jIndex - 1).foreach{ lIndex: Int =>
          val cVal = C(jIndex, lIndex)
          val newVal = (x(index)(jIndex) - meanX(jIndex)) * (x(index)(lIndex) - meanX(lIndex))
          C.update(jIndex, lIndex, cVal + newVal)
        }
      }
    }
    val newtol = tol * tol
    (0 to newp-1).foreach{ jIndex =>
      (0 to jIndex).foreach{ lIndex =>
        C.update(jIndex, lIndex, C(jIndex, lIndex) / (n -newk))
        C.update(lIndex, jIndex, C(lIndex, jIndex) / (n -newk))
      }

      println(s"the matrix is $C")
      println(s"Covariance matrix variable ${C(jIndex, jIndex)} ${newtol}")
      if (C(jIndex, jIndex) < newtol) {
        throw new IllegalArgumentException(s"Covariance matrix (variable $jIndex) is close to singular.")
      }
    }

//    C.setSymmetric(true);
    val evd = eigSym(C)

    for (s <- evd.eigenvalues) {
      if (s < tol) {
        throw new IllegalArgumentException("The covariance matrix is close to singular.")
      }
    }

    val eigen = evd.eigenvalues
    val scaling = evd.eigenvectors
    new LDA(newp, newk, ct, priori, mu, scaling, eigen)
  }

}