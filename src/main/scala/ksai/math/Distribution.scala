package ksai.math


/**
  * Probability distribution of univariate random variable. A probability
  * distribution identifies either the probability of each value
  * of a random variable (when the variable is discrete), or
  * the probability of the value falling within a particular interval (when
  * the variable is continuous). When the random variable takes values in the
  * set of real numbers, the probability distribution is completely described
  * by the cumulative distribution function, whose value at each real x is the
  * probability that the random variable is smaller than or equal to x.
  *
  */
trait Distribution {
  /**
    * The number of parameters of the distribution.
    */
  def npara: Int

  /**
    * The mean of distribution.
    */
  def mean: Double

  /**
    * The variance of distribution.
    */
  def `var`: Double

  /**
    * The standard deviation of distribution.
    */
  def sd: Double

  /**
    * Shannon entropy of the distribution.
    */
  def entropy: Double

  /**
    * Generates a random number following this distribution.
    */
  def rand: Double

  /**
    * The probability density function for continuous distribution
    * or probability mass function for discrete distribution at x.
    */
  def p(x: Double): Double

  /**
    * The density at x in log scale, which may prevents the underflow problem.
    */
  def logp(x: Double): Double

  /**
    * Cumulative distribution function. That is the probability to the left of x.
    */
  def cdf(x: Double): Double

  /**
    * The quantile, the probability to the left of quantile is p. It is
    * actually the inverse of cdf.
    */
  def quantile(p: Double): Double

  /**
    * The likelihood of the sample set following this distribution.
    */
  def likelihood(x: Array[Double]): Double

  /**
    * The log likelihood of the sample set following this distribution.
    */
  def logLikelihood(x: Array[Double]): Double
}

