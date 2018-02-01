package ksai.util

import DoubleUtil._

object NumericFunctions {


  def logisticSigmoid(value: Double): Double = {
    val y = if (value < -40) {
      2.353853e+17
    } else if (value > 40) {
      1.0 + 4.248354e-18;
    } else {
      1.0 + Math.exp(-value);
    }

    1.0 / y
  }

  /**
    * Returns natural log without underflow.
    */
  def log(x: Double): Double = {
    if (x < 1E-300) {
      -690.7755
    } else {
      Math.log(x)
    }
  }

  def squaredDistance(x: List[Double], y: List[Double]): Double = {
    if (x.length != y.length) {
      throw new IllegalArgumentException("Input vector sizes are different.");
    }

    (x zip y).foldLeft(0.0) {
      case (result, (xVallue, yValue)) =>
        val diff = xVallue - yValue
        result + (diff * diff)
    }
  }

  def squaredDistanceWithMissingValues(x: List[Double], y: List[Double]): Double = {
    val (dist, nonMissing) = (x zip y).foldLeft((0.0, 0)) {
      case ((result, nonMissingValue), (xValue, yValue)) =>
        if (!xValue.nan && !yValue.nan) {
          val distance: Double = xValue - yValue
          (result + (distance * distance), nonMissingValue + 1)
        } else {
          (result, nonMissingValue)
        }
    }

    if (dist == 0.0) {
      Double.MaxValue
    } else {
      x.length * dist / nonMissing
    }
  }


  /**
    * Jensen-Shannon divergence JS(P||Q) = (KL(P||M) + KL(Q||M)) / 2, where
    * M = (P+Q)/2. The Jensen-Shannon divergence is a popular
    * method of measuring the similarity between two probability distributions.
    * It is also known as information radius or total divergence to the average.
    * It is based on the Kullback-Leibler divergence, with the difference that
    * it is always a finite value. The square root of the Jensen-Shannon divergence
    * is a metric.
    */
  def jensenShannonDivergence(x: List[Double], y: List[Double]): Double = {
    val m = (x zip y).map {
      case (xValue, yValue) => (xValue + yValue) / 2
    }
    (kullbackLeiblerDivergence(x, m) + kullbackLeiblerDivergence(y, m)) / 2
  }

  /**
    * Kullback-Leibler divergence. The Kullback-Leibler divergence (also
    * information divergence, information gain, relative entropy, or KLIC)
    * is a non-symmetric measure of the difference between two probability
    * distributions P and Q. KL measures the expected number of extra bits
    * required to code samples from P when using a code based on Q, rather
    * than using a code based on P. Typically P represents the "true"
    * distribution of data, observations, or a precise calculated theoretical
    * distribution. The measure Q typically represents a theory, model,
    * description, or approximation of P.
    * <p>
    * Although it is often intuited as a distance metric, the KL divergence is
    * not a true metric - for example, the KL from P to Q is not necessarily
    * the same as the KL from Q to P.
    */
  def kullbackLeiblerDivergence(x: List[Double], y: List[Double]): Double = {
    val (resKL, intersect) = (x zip y).foldLeft((0.0, false)) {
      case ((kl, intersection), (xValue, yValue)) =>
        if (xValue != 0.0 && yValue != 0.0) {
          (kl + (xValue * Math.log(xValue / yValue)), true)

        } else (kl, intersection)
    }

    if (intersect) {
      resKL
    } else {
      Double.PositiveInfinity
    }
  }

  def choose(n: Int, k: Int): Double = {
    if (n < 0 || k < 0) {
      throw new IllegalArgumentException(s"Invalid n = $n, k = $k")
    }

    if (n < k) {
      0.0
    } else {
      Math.floor(0.5 + Math.exp(logChoose(n, k)))
    }
  }

  def logChoose(n: Int, k: Int) = {
    if (n < 0 || k < 0 || k > n) {
      throw new IllegalArgumentException(s"Invalid n = $n, k = $k")
    }

    logFactorial(n) - logFactorial(k) - logFactorial(n - k)
  }

  def logFactorial(n: Int) = {
    if (n < 0) {
      throw new IllegalArgumentException(s"n has to be nonnegative: $n")
    }

    (2 to n).toList.foldLeft(0.0) {
      case (result, cnt) => result + Math.log(cnt)
    }
  }

}
