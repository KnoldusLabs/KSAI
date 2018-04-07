package ksai.core.projection

import breeze.linalg._
import breeze.stats.mean
import breeze.linalg.svd
import breeze.linalg.svd.DenseSVD
import ksai.util.NumericFunctions
import spire.std.double

import scala.collection.mutable.ArrayBuffer


case class PCA(
                p: Int,
                n: Int,
                mu: DenseVector[Double],
                pmu: DenseVector[Double],
                eigvectors: DenseMatrix[Double],
                eigvalues: DenseVector[Double],
                proportion: DenseVector[Double],
                cumulativeProportion: ArrayBuffer[Double],
                projection: DenseMatrix[Double]
              ) {


  def project(x: ArrayBuffer[Double]): DenseVector[Double] = {
    if (x.length != n) {
      throw new IllegalArgumentException(s"Invalid input vector size: ${x.length}, expected: $n")
    }

    val y = projection * DenseVector(x: _*)
    y - pmu
  }

  def project(x: ArrayBuffer[ArrayBuffer[Double]]): DenseMatrix[Double] = {
    if (x(0).length != mu.length) {
      throw new IllegalArgumentException(s"Invalid input vector size: ${x(0).length}, expected: $n")
    }

    val y = DenseMatrix.zeros[Double](x.length, p)
    (0 to x.length - 1).foreach { index =>
      val yVec = projection * DenseVector(x(index): _*)
      val yIndx = yVec - pmu
      y(index, ::) := yIndx.t
    }
    y
  }


}

object PCA {

  def apply(data: ArrayBuffer[ArrayBuffer[Double]], cor: Boolean = false) = {
    val m = data.length
    val n = data(0).length

    val denseX: DenseMatrix[Double] = DenseMatrix(data.map(_.toArray): _*)
    val mu: DenseVector[Double] = mean(denseX(::, *)).inner

    (0 to n - 1).foreach { jIndex =>
      (0 to m - 1).foreach { index =>
        denseX.update(index, jIndex, denseX(index, jIndex) - mu(jIndex))
      }
    }

    val (eigVal, eigVec) = if (m > n && !cor) {
      val svd.SVD(leftVec, singVal, rightVec) = svd(denseX.t)
      val eigvalues: DenseVector[Double] = singVal
      (0 to eigvalues.length - 1).foreach { index =>
        eigvalues(index) = eigvalues(index) * eigvalues(index)
      }

      val eigvectors = rightVec
      (eigvalues, eigvectors)
    }
    else {
      val cov: DenseMatrix[Double] = DenseMatrix.zeros[Double](n, n)
      (0 to m - 1).foreach { kIndex =>
        (0 to n - 1).foreach { index =>
          (0 to index).foreach { jIndex =>
            cov.update(index, jIndex, cov(index, jIndex) + ((denseX(kIndex, index) * denseX(kIndex, jIndex))))
          }
        }
      }

      (0 to n - 1).foreach { index =>
        (0 to index).foreach { jIndex =>
          cov.update(index, jIndex, cov(index, jIndex) / m)
          cov.update(jIndex, index, cov(index, jIndex))
        }
      }

      var sd = new ArrayBuffer[Double]()
      if (cor) {
        sd = ArrayBuffer[Double]((0 to n - 1).map(_ => 0.0): _*)
        (0 to n - 1).foreach { index =>
          sd(index) = Math.sqrt(cov(index, index))
        }

        (0 to n - 1).foreach { index =>
          (0 to index).foreach { jIndex =>
            cov.update(index, jIndex, cov(index, jIndex) / sd(index) * sd(jIndex))
            cov.update(jIndex, index, cov(index, jIndex))
          }
        }
      }

      val eigen = eigSym(cov)

      val loadings = eigen.eigenvectors
      if (cor) {
        (0 to n - 1).foreach { index =>
          (0 to n - 1).foreach { jIndex =>
            loadings.update(index, jIndex, loadings(index, jIndex) / sd(index))
          }
        }
      }

      val eigvalues = eigen.eigenvalues
      val eigvectors = loadings
      (eigvalues, eigvectors)
    }

    val proportion = DenseVector(eigVal.toArray: _*)
    NumericFunctions.unitize1(proportion)

    val cumulativeProportion = ArrayBuffer[Double]((0 to eigVal.length - 1).map(_ => 0.0): _*)
    cumulativeProportion(0) = proportion(0)
    (1 to eigVal.length - 1).foreach { index =>
      cumulativeProportion(index) = cumulativeProportion(index - 1) + proportion(index)
    }

    setProjection(0.95, n, eigVec, eigVal, mu, proportion, cumulativeProportion)
  }

  def setProjection(p: Int, pca: PCA): PCA = {
    setProjection(p, pca.n, pca.eigvectors, pca.eigvalues, pca.mu, pca.proportion, pca.cumulativeProportion)
  }

  def setProjection(p: Int, n: Int, eigvectors: DenseMatrix[Double], eigvalues: DenseVector[Double],
                    mu: DenseVector[Double], proportion: DenseVector[Double], cumulativeProportion: ArrayBuffer[Double]): PCA = {
    if (p < 1 || p > n) {
      throw new IllegalArgumentException("Invalid dimension of feature space: " + p)
    }

    val projection = DenseMatrix.zeros[Double](p, n)
    (0 to n - 1).foreach { index =>
      (0 to p - 1).foreach { jIndex =>
        projection.update(jIndex, index, eigvectors(index, jIndex))
      }
    }

    val pmu = projection * mu
    new PCA(p, n, mu, pmu, eigvectors, eigvalues, proportion, cumulativeProportion, projection)
  }

  def setProjection(p: Double, n: Int, eigvectors: DenseMatrix[Double], eigvalues: DenseVector[Double],
                    mu: DenseVector[Double], proportion: DenseVector[Double], cumulativeProportion: ArrayBuffer[Double]): PCA = {
    if (p <= 0 || p > 1) {
      throw new IllegalArgumentException("Invalid percentage of variance: " + p)
    }

    val pca = new PCA(1, n, mu, DenseVector[Double](), eigvectors, eigvalues, proportion,
      cumulativeProportion, DenseMatrix.zeros[Double](1, n))
    val (result, _) = (0 to n - 1).foldLeft((pca, true)) { case ((result, check), kIndex) =>
      if (check && cumulativeProportion(kIndex) >= p) {
        val pc = setProjection(kIndex + 1, n, eigvectors, eigvalues, mu, proportion, cumulativeProportion)
        (pc, false)
      } else {
        (result, check)
      }
    }
    result
  }


}