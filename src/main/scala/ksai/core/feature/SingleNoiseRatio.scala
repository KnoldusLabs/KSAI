package ksai.core.feature

import breeze.linalg._
import breeze.stats.{mean, stddev}
import spire.std.double

import scala.collection.mutable.ArrayBuffer


object SingleNoiseRatio {

  def rank(x: ArrayBuffer[ArrayBuffer[Double]], y: ArrayBuffer[Int]): ArrayBuffer[Double] = {
    if (x.length != y.length) {
      throw new IllegalArgumentException(s"The sizes of X and Y don't match: ${x.length} != ${y.length}")
    }

    var n1 = 0
    for (yi <- y) {
      if (yi == 0) {
        n1 = n1 + 1
      } else if (yi != 1) {
        throw new IllegalArgumentException("Invalid class label: " + yi)
      }
    }

    val n = x.length
    val n2 = n - n1
    val x1 = ArrayBuffer[ArrayBuffer[Double]]((0 to n1-1).map(_ => new ArrayBuffer[Double]()):_*)
    val x2 = ArrayBuffer[ArrayBuffer[Double]]((0 to n2-1).map(_ => new ArrayBuffer[Double]()):_*)
    var j = 0
    var k = 0
    ( 0 to n-1).foreach{ index =>
      if (y(index) == 0) {
        x1(j) = x(index)
        j = j +1
      } else {
        x2(k) = x(index)
        k = k+1
      }
    }

    val denseX1: DenseMatrix[Double] = DenseMatrix(x1.map(_.toArray):_*)
    val denseX2: DenseMatrix[Double] = DenseMatrix(x2.map(_.toArray):_*)

    val mu1: DenseVector[Double] = mean(denseX1(::, *)).inner
    val mu2: DenseVector[Double] = mean(denseX2(::, *)).inner
    val sd1: DenseVector[Double] = stddev(denseX1(::, *)).inner
    val sd2: DenseVector[Double] = stddev(denseX2(::, *)).inner

    val p = mu1.length
    val s2n = ArrayBuffer[Double]((0 to p-1).map(_ => 0.0):_*)
    (0 to p-1).foreach{ index =>
      s2n(index) = Math.abs(mu1(index) - mu2(index)) / (sd1(index) + sd2(index))
    }
    s2n
  }

}
