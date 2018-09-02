package ksai.util

import java.util

import breeze.linalg.DenseVector
import ksai.math.{DifferentiableMultivariateFunction, MultivariateFunction}
import ksai.util.DoubleUtil._


object NumericFunctions {

  private val EPSILON = Math.pow(2.0, -52.0)

  def logisticSigmoid(value: Double): Double = {
    val y = if (value < -40) {
      2.353853e+17
    } else if (value > 40) {
      1.0 + 4.248354e-18
    } else {
      1.0 + Math.exp(-value)
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
      throw new IllegalArgumentException("Input vector sizes are different.")
    }

    (x zip y).foldLeft(0.0) {
      case (result, (xValue, yValue)) =>
        val diff = xValue - yValue
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

  def logChoose(n: Int, k: Int): Double = {
    if (n < 0 || k < 0 || k > n) {
      throw new IllegalArgumentException(s"Invalid n = $n, k = $k")
    }

    logFactorial(n) - logFactorial(k) - logFactorial(n - k)
  }

  def logFactorial(n: Int): Double = {
    if (n < 0) {
      throw new IllegalArgumentException(s"n has to be nonNegative: $n")
    }

    (2 to n).toList.foldLeft(0.0) {
      case (result, cnt) => result + Math.log(cnt)
    }
  }

  def unitize1(x: DenseVector[Double]): Unit = {
    val n = norm1(x)

    (0 until x.length).foreach{ index =>
      x(index) = x(index) / n
    }
  }

  def norm1(x: DenseVector[Double]): Double = {
    var norm = 0.0
    for (n <- x.toArray) {
      norm = norm + Math.abs(n)
    }
    norm
  }

  def norm2(x: DenseVector[Double]): Double = {
    var norm = 0.0
    for (n <- x.toArray) {
      norm = norm + n * n
    }
    Math.sqrt(norm)
  }

  def round (value: Double, precision: Int): Double = {
    val scale = Math.pow(10, precision).toInt
    (Math.round(value * scale) / scale).toDouble
  }

  def sqr(x: Double): Double = x * x

  /**
    * Returns the dot product between two vectors.
    */
  def dot(x: Array[Double], y: Array[Double]): Double = {
    if (x.length != y.length) throw new IllegalArgumentException("Arrays have different length.")
    x.indices.foldLeft(0.0){(sum, i) => sum + x(i) * y(i)}
  }

  private def axpy(a: Double, x: Array[Double], y: Array[Double]): Array[Double] = {
    if (x.length != y.length) {
      throw new IllegalArgumentException(s"Arrays have different length: ${x.length}, ${y.length}")
    }
    x.indices.foreach(i => y(i) = y(i) + a * x(i))
    y
  }

  def lineSearch(multivariateFunction: MultivariateFunction, xOld: Array[Double], fOld: Double,
                 g: Array[Double], p: Array[Double], x: Array[Double], stpMax: Double): Double = {
    if (stpMax <= 0) {
      throw new IllegalArgumentException("Invalid upper bound of linear search step: " + stpMax)
    }

    val xTol = EPSILON
    val fTol = 1.0E-4
    val pNorm = NumericFunctions.norm2(DenseVector(p))//norm2(p)
    val indices = xOld.indices

    if(pNorm > stpMax){
      val r = stpMax / pNorm
      indices.foreach(i => p(i) = p(i) * r)
    }

    val slope = indices
      .foldLeft(0.0){(sum, i) => sum + g(i) * p(i)}

    if (slope >= 0) {
      throw new IllegalArgumentException("Line Search: the search direction is not a descent direction, which may be caused by roundOff problem.")
    }

    val test = indices.foldLeft(0.0){(t, i) =>
      val temp = Math.abs(p(i)) / Math.max(xOld(i), 1.0)
      if(temp > t) temp else t
    }
    val alammin = xTol / test
    val alam = 1.0
    val (alam2, f2) = (0.0, 0.0)
    converge(multivariateFunction, alam, alam2, alammin, xOld, fOld, x, p, fTol, slope, f2)
  }

  /**helper method for lineSearch*/
  private def converge(func: MultivariateFunction,
                       alam: Double,
                       alam2: Double,
                       alammin : Double,
                       xOld: Array[Double],
                       fOld: Double,
                       x: Array[Double],
                       p: Array[Double],
                       fTol: Double, slope: Double, f2: Double): Double ={
    val indices = x.indices
    indices.foreach(i => x(i) = xOld(i) + alam * p(i))
    val f = func.f(x)
    if(alam < alammin) {
      System.arraycopy(xOld, 0, x, 0, xOld.length)
      f
    } else if(f <= fOld + fTol * alam * slope){
      f
    } else{
      // Backtrack
      val tmpalam = if (alam == 1.0) {
        -slope / (2.0 * (f - fOld - slope))
      } else{
        val rhs1 = f - fOld - alam * slope
        val rhs2 = f2 - fOld - alam2 * slope
        val a = (rhs1 / (alam * alam) - rhs2 / (alam2 * alam2)) / (alam - alam2)
        val b = (-alam2 * rhs1 / (alam * alam) + alam * rhs2 / (alam2 * alam2)) / (alam - alam2)

        if (a == 0.0) {
          -slope / (2.0 * b)
        } else {
          val disc = b * b - 3.0 * a * slope
          val temp = if (disc < 0.0) {
            0.5 * alam
          } else if (b <= 0.0) {
            (-b + Math.sqrt(disc)) / (3.0 * a)
          } else {
            -slope / (b + Math.sqrt(disc))
          }
          if(temp > 0.5 * alam) 0.5 * alam else temp
        }
      }
      converge(func, Math.max(tmpalam, 0.1 * alam), alam, alammin, xOld, fOld, x, p, fTol, slope, f)
    }
  }

  /**
    * Logistic sigmoid function.
    */
  def logistic(x: Double): Double = {
    val y = if (x < -40) 2.353853e+17 else if (x > 40) 1.0 + 4.248354e-18 else 1.0 + Math.exp(-x)
    1.0 / y
  }

  /**
    * This method solves the unconstrained minimization problem
    * <pre>
    * min f(x),    x = (x1,x2,...,x_n),
    * </pre>
    * using the limited-memory BFGS method. This method is the optimization of
    * BFGS minimization algorithms. It is used for the problems that are too
    * big for limited memory computer systems. i.e. the problems with a large
    * variables set.
    *
    * @param func                 the function to be minimized.
    * @param m                    the number of corrections used in the L-BFGS
    *                             update. General value = 5, should not be less
    *                             than 3 and not too high(more computation time)
    * @param x                    initial estimate of the solution vector and after
    *                             execution get refined at the best point found
    * @param gradientTolerance    the convergence requirement on zeroing the gradient.
    * @param maxIteration         the maximum allowed number of iterations.
    * @return                     the minimum value of the function.
    */
  def min(func: DifferentiableMultivariateFunction,
          m: Int,
          x: Array[Double],
          gradientTolerance : Double,
          maxIteration: Int): Double = {
    if (m <= 0) throw new IllegalArgumentException("Invalid m: " + m)

    val TOLX = 4 * EPSILON
    val STPMX = 100.0
    val n = x.length
    val xNew = new Array[Double](n)// The solution vector of line search.
    val gNew = new Array[Double](n)// The gradient vector of line search.
    val xi = new Array[Double](n)// Line search direction.
    val s = new Array[Array[Double]](m).map(_ => Array.fill(n)(0.0)) // difference of x from previous step.
    val y = new Array[Array[Double]](m).map(_ => Array.fill(n)(0.0)) // difference of g from previous step.
    val rho = new Array[Double](m)// buffer for 1 / (y' * s)
    val a = new Array[Double](m)
    val g = new Array[Double](n)// Current gradient.

    val fOld = func.f(x, g)

    println(f"L-BFGS: initial function value: $fOld%.5g")

    val sum = (0 until n).foldLeft(0.0){(s, i) =>
      xi(i) = -g(i)
      s + x(i) * x(i)
    }

    // Initial line search direction.
    // Upper limit for line search step.
    val stpMax = STPMX * Math.max(Math.sqrt(sum), n)

    val (_, complete, res) = (1 to maxIteration).foldLeft((0, false, fOld)){
      case ((k, done, f), _) if done => (k, done, f)
      case ((k, _, lastF), iteration) =>


        lineSearch(func, x, lastF, g, xi, xNew, stpMax)
        val f = func.f(xNew, gNew)
        val indices = x.indices

        indices.foreach{ i =>
          s(k)(i) = xNew(i) - x(i)
          y(k)(i) = gNew(i) - g(i)
          x(i) = xNew(i)
          g(i) = gNew(i)
        }

        val test = indices.foldLeft(0.0){(oldTest, i) =>
          val temp = Math.abs(s(k)(i)) / Math.max(Math.abs(x(i)), 1.0)
          if(temp > oldTest) temp else oldTest
        }

        if (test < TOLX) {
          println(f"L-BFGS: the function value after $iteration%3d iterations: $f%.5g")
          (k, true, f)
        } else {
          val den = Math.max(f, 1.0)

          val test2 = indices.foldLeft(0.0){(oldTest, i) =>
            val temp = Math.abs(g(i)) * Math.max(Math.abs(x(i)), 1.0) / den
            if(temp > oldTest) temp else oldTest
          }

          if(test2 < gradientTolerance) {
            println(f"L-BFGS: the function value after $iteration%3d iterations: $f%.5g")
            (k, true, f)
          } else {
            if (iteration % 10 == 0) println(f"L-BFGS: the function value after $iteration%3d iterations: $f%.5g")

            val ys = dot(y(k), s(k))
            val yy = dot(y(k), y(k))
            val diag = ys / yy
            rho(k) = 1.0 / ys

            indices.foreach(i => xi(i) = -g(i))
            val bound = if(iteration > m) m else iteration

            val cp = (0 until bound).foldLeft(k){(old, i) =>
              a(old) = rho(old) * dot(s(old), xi)
              axpy(-a(old), y(old), xi)
              if((old - 1) == -1) m - 1 else old - 1
            }

            indices.foreach(i => xi(i) = xi(i) * diag)

            (0 until bound).foldLeft(cp){(cpi, i) =>
              val ncp = if(cpi + 1 == m) 0 else cpi + 1
              val b = rho(ncp) * dot(y(ncp), xi)
              axpy(a(ncp) - b , s(ncp), xi)
              ncp
            }
            val newK = if(k + 1 == m) 0 else k + 1
            (newK, false, f)
          }
        }
    }
    if(complete) res else throw new IllegalStateException("L-BFGS: Too many iterations.")
  }

  /**
    * This method solves the unconstrained minimization problem
    * <pre>
    * min f(x),    x = (x1,x2,...,x_n),
    * </pre>
    * using the BFGS method.
    *
    * @param func                 the function to be minimized.
    * @param x                    initial estimate of the solution vector and after
    *                             execution get refined at the best point found
    * @param gradientTolerance    the convergence requirement on zeroing the gradient.
    * @param maxIteration         the maximum allowed number of iterations.
    * @return                     the minimum value of the function.
    */
  def min(func: DifferentiableMultivariateFunction,
          x: Array[Double],
          gradientTolerance : Double,
          maxIteration: Int): Double = {

    val TOLX = 4 * EPSILON
    val STPMX = 100.0

    val n = x.length
    val dg = new Array[Double](n)
    val g = new Array[Double](n)
    val hdg = new Array[Double](n)
    val xNew = new Array[Double](n)
    val xi = new Array[Double](n)
    val hessin = new Array[Array[Double]](n).map(_ => Array.fill(n)(0.0))

    // Calculate starting function value and gradient and initialize the
    // inverse Hessian to the unit matrix.
    val fOld = func.f(x, g)

    val sum = (0 until n).foldLeft(0.0){(s, i) =>
      hessin(i)(i) = 1.0
      xi(i) = -g(i)
      s + x(i) * x(i)
    }

    val stpMax = STPMX * Math.max(Math.sqrt(sum), n)

    println(f"BFGS: initial function value: $fOld%.5g")

    val (complete, res) = (1 to maxIteration).foldLeft((false, fOld)){
      case ((done, f), _) if done=> (done, f)
      case ((_, lastF), iteration) =>

        val f = lineSearch(func, x, lastF, g, xi, xNew, stpMax)

        if (iteration % 10 == 0)
          println(f"BFGS: the function value after $iteration%3d iterations: $f%.5g")

        (0 until n).foreach{i =>
          xi(i) = xNew(i) - x(i)
          x(i) = xNew(i)
        }

        val indices = x.indices

        val test = indices.foldLeft(0.0){(oldTest, i) =>
          val temp = Math.abs(xi(i)) / Math.max(Math.abs(x(i)), 1.0)
          if(temp > oldTest) temp else oldTest
        }

        if (test < TOLX) {
          println(f"BFGS: the function value after $iteration%3d iterations: $f%.5g")
          (true, f)
        } else {

          System.arraycopy(g, 0, dg, 0, n)
          func.f(x, g)

          val den = Math.max(f, 1.0)

          val test2 = indices.foldLeft(0.0){(oldTest, i) =>
            val temp = Math.abs(g(i)) * Math.max(Math.abs(x(i)), 1.0) / den
            if(temp > oldTest) temp else oldTest
          }

          if(test2 < gradientTolerance) {
            println(f"BFGS: the function value after $iteration%3d iterations: $f%.5g")
            (true, f)
          } else {

            indices.foreach(i => dg(i) = g(i) - dg(i))

            indices.foreach{i =>
              hdg(i) = 0
              indices.foreach{j =>
                hdg(i) = hdg(i) + (hessin(i)(j) * dg(j))
              }
            }

            val initial = 0.0

            val (faC, faE, sumDg, sumXi) = indices.foldLeft((initial, initial, initial, initial)){
              case ((fac, fae, sumdg, sumxi), i) =>
                (
                  fac + dg(i) * xi(i),
                  fae + dg(i) * hdg(i),
                  sumdg + dg(i) * dg(i),
                  sumxi + xi(i) * xi(i)
                )
            }

            // Skip update if fac is not sufficiently positive.
            if (faC > Math.sqrt(EPSILON * sumDg * sumXi)) {
              val fac = 1.0 / faC
              val fad = 1.0 / faE

              indices.foreach(i => dg(i) = fac * xi(i) - fad * hdg(i))

              for (i <- 0 until n; j <- i until n){
                hessin(i)(j) = hessin(i)(j) + (fac * xi(i) * xi(j) - fad * hdg(i) * hdg(j) + faE * dg(i) * dg(j))
                hessin(j)(i) = hessin(i)(j)
              }
            }

            // Calculate the next direction to go.
            util.Arrays.fill(xi, 0.0)
            for (i <- 0 until n; j <- 0 until n) {
              xi(i) = xi(i) - hessin(i)(j) * g(j)
            }

            (false, f)
          }
        }
    }

    if(complete) res else throw new IllegalStateException("BFGS: Too many iterations.")
  }

}
