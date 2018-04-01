package ksai.core.feature

import scala.collection.mutable.ArrayBuffer


object SumSquareRatio {

  def rank(x: ArrayBuffer[ArrayBuffer[Double]], y: ArrayBuffer[Int]): ArrayBuffer[Double] = {
    if (x.length != y.length) {
      throw new IllegalArgumentException(s"The sizes of X and Y don't match: ${x.length} != ${y.length}")
    }
    // class label set.
    val labels = y.distinct.sortWith{case (first, second) => first < second}

    (0 to labels.length - 1).foreach { index =>
      if (labels(index) < 0) {
        throw new IllegalArgumentException("Negative class label: " + labels(index))
      }

      if (index > 0 && labels(index) - labels(index -1) > 1) {
        throw new IllegalArgumentException("Missing class: " + labels(index)+1)
      }
    }

    val k = labels.length
    if (k < 2) {
      throw new IllegalArgumentException("Only one class.")
    }

    val n = x.length
    val p = x(0).length
    val nc = ArrayBuffer[Int]((0 to k-1).map(_ => 0):_*)
    val mu = ArrayBuffer[Double]((0 to p-1).map(_ => 0.0):_*)
    val condmu = ArrayBuffer[ArrayBuffer[Double]]((0 to k -1).map(_ => ArrayBuffer[Double]((0 to p-1).map(_ => 0.0):_*)):_*)

    (0 to n-1).foreach {index =>
      val yi = y(index)
      nc(yi) = nc(yi) + 1
      (0 to p-1).foreach {jIndex =>
        mu(jIndex) = mu(jIndex) + x(index)(jIndex)
        condmu(yi)(jIndex) = condmu(yi)(jIndex) + x(index)(jIndex)
      }
    }

    (0 to p-1).foreach {jIndex =>
      mu(jIndex) = mu(jIndex)/ n
      (0 to k-1).foreach {index =>
        condmu(index)(jIndex) = condmu(index)(jIndex) / nc(index)
      }
    }

    val wss = ArrayBuffer[Double]((0 to p-1).map(_ => 0.0):_*)
    val bss = ArrayBuffer[Double]((0 to p-1).map(_ => 0.0):_*)

    (0 to n-1).foreach {index =>
      val yi = y(index)
      (0 to p-1).foreach {jIndex =>
        bss(jIndex) = bss(jIndex) + ((condmu(yi)(jIndex) - mu(jIndex)) * (condmu(yi)(jIndex) - mu(jIndex)))
        wss(jIndex) = wss(jIndex) + ((x(index)(jIndex) - condmu(yi)(jIndex)) * (x(index)(jIndex) - condmu(yi)(jIndex)))
      }
    }

    (0 to p-1).foreach {jIndex =>
      bss(jIndex) = bss(jIndex) /  wss(jIndex)
    }
    bss
  }

}
