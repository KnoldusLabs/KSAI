package ksai.core.projection

import breeze.linalg._
import breeze.stats.mean
import breeze.linalg.svd
import ksai.util.NumericFunctions

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
              )

object PCA {

  def apply(data: ArrayBuffer[ArrayBuffer[Double]], cor: Boolean) = {
    val m = data.length
    val n = data(0).length

    val denseX: DenseMatrix[Double] = DenseMatrix(data.map(_.toArray): _*)
    val mu: DenseVector[Double] = mean(denseX(::, *)).inner
    //    val mu = Math.colMeans(data)
    //    DenseMatrix x = Matrix.newInstance(data);
    (0 to n - 1).foreach { index =>
      (0 to m - 1).foreach { jIndex =>
        denseX.update(index, jIndex, denseX(index, jIndex) / mu(jIndex))
      }
    }

    val (eigVal, eigVec) = if (m > n && !cor) {
      val svdc = svd(denseX)
      val eigvalues = svdc.singularValues
      (0 to eigvalues.length - 1).foreach { index =>
        eigvalues(index) = eigvalues(index) * eigvalues(index)
      }

      val eigvectors = svdc.Vt
      (eigvalues, eigvectors)
    } else {
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
    (0 to eigVal.length - 1).foreach { index =>
      cumulativeProportion(index) = cumulativeProportion(index - 1) + proportion(index)
    }

    setProjection(0.95,k, n, eigVec, eigVal, mu, proportion, cumulativeProportion)
  }


  def setProjection(p: Int, n: Int, eigvectors: DenseMatrix[Double], eigvalues: DenseVector[Double],
                    mu: DenseVector[Double], proportion: DenseVector[Double], cumulativeProportion: DenseVector[Double]) = {
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

  def setProjection(p: Double, k: Int, n: Int, eigvectors: DenseMatrix[Double], eigvalues: DenseVector[Double],
                    mu: DenseVector[Double], proportion: DenseVector[Double], cumulativeProportion: DenseVector[Double]) = {
    if (p <= 0 || p > 1) {
      throw new IllegalArgumentException("Invalid percentage of variance: " + p)
    }

    val pca = new PCA(p, n, mu, DenseVector[Double](), eigvectors, eigvalues, proportion, cumulativeProportion, DenseMatrix[Double]())
    val (result, _) = (0 to k - 1).foldLeft((pca, true)) { case ((result, check), kIndex) =>
      if (check && cumulativeProportion(kIndex) >= p) {
        val pc = setProjection(k + 1, n, eigvectors, eigvalues, mu, proportion, cumulativeProportion)
        (pc, false)
      } else {
        (result, check)
      }
    }
    result
  }

}