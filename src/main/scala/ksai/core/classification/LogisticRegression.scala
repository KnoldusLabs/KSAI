package ksai.core.classification

import java.util
import java.util.concurrent.Executors

import ksai.core.classification.MulticoreExecutor.IO._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.Try

class LogisticRegression(
                          trainingInstances: Array[Array[Double]],
                          responses: Array[Int],
                          lambda: Double = 0.0,
                          tol: Double = 1E-5,
                          maxIterations: Int = 500,
                          logLikelihood: Double = 0.0,
                          linearWeights: Array[Double] = Array.empty[Double], //w
                          linearWeightsMultiClass: Array[Array[Double]] = Array.empty[Array[Double]] //W
                        )
  extends SoftClassifier[Array[Double]] {

  private val labels = responses.distinct
  private val labelCount: Int = labels.length //k
  private val inputLength: Int = trainingInstances.head.length //p

  override def predict(x: Array[Double]): Int = predict(x, Array.empty[Double])

  override def predict(x: Array[Double], posterior: Array[Double]): Int = {
    if (x.length != inputLength) {
      throw new IllegalArgumentException(s"Invalid input vector size: ${x.length}, expected: $inputLength")
    }

    if (!(posterior sameElements Array.empty[Double]) && posterior.length != labelCount) {
      throw new IllegalArgumentException(s"Invalid posteriori vector size: ${posterior.length}, expected: $labelCount")
    }
    if(labelCount == 2){
      val f = 1.0 / (1.0 + Math.exp(-LogisticRegression.dot(x, linearWeights)))
      if(!(posterior sameElements Array.empty[Double])){
        posterior(0) = 1.0 - f
        posterior(1) = f
      }
      if (f < 0.5){
        0
      } else {
        1
      }
    } else {
      val l = -1
      val (label, max) = (0 until labelCount).foldLeft((l, Double.NegativeInfinity)) {
        case ((res, tempMax), i) =>
          val prob = LogisticRegression.dot(x, linearWeightsMultiClass(i))
          if (!(posterior sameElements Array.empty[Double])) {
            posterior(i) = prob
          }
          if (prob > tempMax) {
            (i, prob)
          } else {
            (res, tempMax)
          }
      }
      if (!(posterior sameElements Array.empty[Double])) {
        val Z = (0 until labelCount).foldLeft(0.0){(z, i) =>
          posterior(i) = Math.exp(posterior(i) - max)
          z + posterior(i)
        }

        (0 until labelCount).foreach{i =>
          posterior(i) = posterior(i) / Z
        }
      }
      label
    }
  }

}

object LogisticRegression {

  private val EPSILON = Math.pow(2.0, -52.0)

  def apply(trainingInstances: Array[Array[Double]],
            responses: Array[Int],
            lambda: Double = 0.0,
            tol: Double = 1E-5,
            maxIterations: Int = 500): LogisticRegression = {
    require(trainingInstances.nonEmpty, "input data must have something to train")
    require(trainingInstances.length == responses.length, s"The sizes of X and Y don't match: ${trainingInstances.length} != ${responses.length}")
    require(lambda >= 0.0, "Invalid regularization factor: " + lambda)
    require(tol > 0.0, "Invalid tolerance: " + tol)
    require(maxIterations > 0, "Invalid maximum number of iterations: " + maxIterations)
    val labels = responses.distinct
    util.Arrays.sort(labels)
    require(!labels.exists(_ < 0), s"Found negative class label: ${labels.filter(_ < 0).mkString(", ")}")
    val (_, missedClasses) = labels.foldLeft((List.empty[Int], List.empty[Int])) {
      case ((Nil, missList), label) => (label :: Nil, missList)
      case ((head :: tail, missList), label) if label - head == 1 => (label :: head :: tail, missList)
      case ((head :: tail, missList), _) => (head :: tail, head + 1 :: missList)
    }
    require(missedClasses.isEmpty, s"Missing class: ${missedClasses.mkString(", ")}")
    require(labels.length >= 2, "Only one class.")
    require(labels.length == 2, "For now supporting only two classes")

    if(labels.length == 2){
      val func = BinaryObjectiveFunction(trainingInstances, responses, lambda)
      val p = trainingInstances.head.length
      val w = new Array[Double](p + 1)
      val L = try{
        LogisticRegression.min(func, 5, w, tol, maxIterations)
      }catch {
        case ex: Throwable => throw ex
      }
      new LogisticRegression(trainingInstances, responses, lambda, tol, maxIterations, L, w)
    } else {
      new LogisticRegression(trainingInstances, responses, lambda, tol, maxIterations)
    }
  }

  def log1pe(x: Double): Double = {
    val y = 0.0
    if (x > 15) x
    else y + Math.log1p(Math.exp(x))
  }

  private def log(x: Double): Double = {
    if(x < 1E-300) {
      -690.7755
    } else {
      Math.log(x)
    }
  }

  private def softMax(prob: Array[Double]): Unit = {
    val  max = prob.indices.foldLeft(Double.NegativeInfinity){
      case (tempMax, i) if prob(i) > tempMax => prob(i)
      case (tempMax, _) => tempMax
    }

    val Z = prob.indices.foldLeft(0.0){(z, i) =>
      val p = Math.exp(prob(i) - max)
      prob(i) = p
      z + p
    }

    prob.indices.foreach(i => prob(i) = prob(i) / Z)
  }

  def dot(x: Array[Double], w: Array[Double], pos: Int): Double = {
    val indices = x.indices
    indices.map(i => x(i) + w(pos + i)).sum + w(pos + indices.max)
  }

  def dot(x: Array[Double], w: Array[Double]): Double = {
    val indices = x.indices
    indices.map(i => x(i) * w(i)).sum
  }

  def logistic(x: Double): Double = {
    val y = if (x < -40) 2.353853e+17 else if (x > 40) 1.0 + 4.248354e-18 else 1.0 + Math.exp(-x)
    1.0 / y
  }

  def min(func: DifferentiableMultivariateFunction, m: Int, x: Array[Double], gTol : Double, maxIteration: Int): Double = {
    if (m <= 0) {
      throw new IllegalArgumentException("Invalid m: " + m)
    }

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
    val sum = (0 until n).map{i =>
      xi(i) = -g(i)
      x(i) * x(i)
    }.sum

    // Initial line search direction.
    // Upper limit for line search step.
    val stpMax = STPMX * Math.max(Math.sqrt(sum), n)
    val (_, complete, res) = (1 to maxIteration).foldLeft((0, false, fOld)){
      case ((k, done, f), _) if done=> (k, done, f)
      case ((k, _, lastF), iter) =>

        lineSearch(func, x, lastF, g, xi, xNew, stpMax, iter)
        val f: Double = func.f(xNew, gNew)
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
          println("L-BFGS: the function value after %3d iterations: %.5g", iter, f)
          (k, true, f)
        } else {
          val den = Math.max(f, 1.0)

          val test2 = indices.foldLeft(0.0){(oldTest, i) =>
            val temp = Math.abs(g(i)) * Math.max(Math.abs(x(i)), 1.0) / den
            if(temp > oldTest) temp else oldTest
          }

          if(test2 < gTol) {
            println("L-BFGS: the function value after %3d iterations: %.5g", iter, f)
            (k, true, f)
          } else {
            if (iter % 10 == 0) {
              println("L-BFGS: the function value after %3d iterations: %.5g", iter, f)
            }

            val ys = dot(y(k), s(k))
            val yy = dot(y(k), y(k))
            val diag = ys / yy
            rho(k) = 1.0 / ys

            indices.foreach(i => xi(i) = -g(i))
            val bound = if(iter > m) m else iter

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

  private def axpy(a: Double, x: Array[Double], y: Array[Double]): Array[Double] = {
    if (x.length != y.length) {
      throw new IllegalArgumentException(s"Arrays have different length: ${x.length}, ${y.length}")
    }
    x.indices.foreach(i => y(i) = y(i) + a * x(i))
    y
  }

  def lineSearch(multivariateFunction: MultivariateFunction, xOld: Array[Double], fOld: Double, g: Array[Double], p: Array[Double], x: Array[Double], stpMax: Double, k: Int): Double = {
    if (stpMax <= 0) {
      throw new IllegalArgumentException("Invalid upper bound of linear search step: " + stpMax)
    }
    val xTol = EPSILON
    val fTol = 1.0E-4
//    val n = xOld.length

    val pNorm = norm2(p)
    val indices = xOld.indices
    if(pNorm > stpMax){
      val r = stpMax / pNorm
      indices.foreach(i => p(i) = p(i) * r)
    }

    val slope = indices.foldLeft(0.0){(sum, i) => sum + g(i) * p(i)}

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

  def converge(multivariateFunction: MultivariateFunction,
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
    val f = multivariateFunction.f(x)
    if(alam < alammin) {
      System.arraycopy(xOld, 0, x, 0, xOld.length)
      f
    } else if(f <= fOld + fTol * alam * slope){
      f
    } else{
      // Backtrack
      val tmpalam = if (alam == 1.0) {
        // First time
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
      converge(multivariateFunction, Math.max(tmpalam, 0.1 * alam), alam, alammin, xOld, fOld, x, p, fTol, slope, f)
    }
  }

  def norm2(x: Array[Double]): Double = {
    val norm = x.foldLeft(0.0){(sum, elem) => sum + elem * elem}
    Math.sqrt(norm)
  }
}

trait MultivariateFunction {
  def f(x: Array[Double]): Double
}

trait DifferentiableMultivariateFunction extends MultivariateFunction{
  def f(x: Array[Double], gradient: Array[Double]): Double
}

class BinaryObjectiveFunction(trainingInstances: Array[Array[Double]],
                              responses: Array[Int],
                              lambda: Double = 0.0,
                              fTask: ArrayBuffer[BinaryObjectiveFunction.FTask],
                              gTask: ArrayBuffer[BinaryObjectiveFunction.GTask]) extends DifferentiableMultivariateFunction{

  def f(w: Array[Double], g: Array[Double]): Double = {
    val p = w.length - 1
    util.Arrays.fill(g, 0.0)
    val value = (gTask.toList match {
      case Nil => Future {Double.NaN}
      case _ =>
        gTask.map(_.copy(w = w))
        Future {
          gTask.map(_.call)
        }.map {
          _.map { gi =>
            w.indices.foreach(i => g(i) = g(i) + gi(i))
            gi(w.length)
          }.sum
        }.recover {
          case _ => Double.NaN
        }
    }).map { res =>

      val tempF = if(res == Double.NaN){
        val n = trainingInstances.length
        (0 until n).foldLeft(0.0){(sum, i) =>
          val wx = LogisticRegression.dot(trainingInstances(i), w)
          val yi = responses(i) - LogisticRegression.logistic(wx)
          (0 until p).foreach(j => g(i) = g(i) - yi * trainingInstances(i)(j))
          g(p) = g(p) - yi
          sum + LogisticRegression.log1pe(wx) - responses(i) * wx
        }
      } else{
        res
      }

      val f = if (lambda != 0.0) {
        val w2 = (0 until p).foldLeft(0.0){(sum, i) => sum + w(i) * w(i)}
        (0 until p).foreach(j => g(j) = g(j) + lambda * w(j))
        tempF + 0.5 * lambda * w2
      } else {
        tempF
      }
      f
    }
    Await.result(value, 10.seconds)
  }

  override def f(w: Array[Double]): Double = {
    val p = w.length - 1

    val value = (fTask.toList match {
      case Nil => Future {Double.NaN}
      case _ =>
        fTask.map(_.copy(w = w))
        Future {
          fTask.map(_.call)
        }.map {
          _.sum
        }.recover {
          case _ => Double.NaN
        }
    }).map { res =>

      val tempF = if(res == Double.NaN){
        val n = trainingInstances.length
        (0 until n).foldLeft(0.0){(sum, i) =>
          val wx = LogisticRegression.dot(trainingInstances(i), w)
          sum + LogisticRegression.log1pe(wx) - responses(i) * wx
        }
      } else{
        res
      }

      val f = if (lambda != 0.0) {
        val w2 = (0 until p).foldLeft(0.0){(sum, i) => sum + w(i) * w(i)}
        tempF + 0.5 * lambda * w2
      } else {
        tempF
      }
      f
    }
    Await.result(value, 10.seconds)
  }
}

object BinaryObjectiveFunction {
  def apply(trainingInstances: Array[Array[Double]], responses: Array[Int], lambda: Double = 0.0): BinaryObjectiveFunction = {
    val nProcessors: Int = getThreadPoolSize //m
    val instanceCount: Int = trainingInstances.length //n
    val fTask = new ArrayBuffer[FTask]()
    val gTask = new ArrayBuffer[GTask]()

    if (instanceCount >= 1000 && nProcessors >= 2) {
      val step = if (instanceCount / nProcessors > 100) instanceCount / nProcessors else 100
      val (lStart, _) = (0 until nProcessors).foldLeft((0, step)) {
        case ((start, end), _) =>
          fTask.append(FTask(start, end, trainingInstances, responses))
          gTask.append(GTask(start, end, trainingInstances, responses))
          (start + step, end + step)
      }
      fTask.append(FTask(lStart, instanceCount, trainingInstances, responses))
    }
    new BinaryObjectiveFunction(trainingInstances, responses, lambda, fTask, gTask)
  }

  case class FTask(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], w: Array[Double] = Array.empty[Double]){

    def call: Double = start until end map { i =>
      val wx = LogisticRegression.dot(x(i), w)
      LogisticRegression.log1pe(wx) - y(i) * wx
    } sum
  }

  case class GTask(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], w: Array[Double] = Array.empty[Double]) {

    def call: Array[Double] = {
      val p = w.length - 1
      val g = new Array[Double](w.length + 1)

      val f: immutable.Seq[Double] = (start until end).map { i =>
        val wx = LogisticRegression.dot(x(i), w)
        val yi = y(i) - LogisticRegression.logistic(wx)
        (0 until p).foreach { j => g(j) = g(j) - yi * x(i)(j) }
        g(p) = g(p) - yi
        LogisticRegression.log1pe(wx) - y(i) * wx
      }

      g(p + 2) = f.sum
      g
    }
  }

}

/*
class MultiClassObjectiveFunction(trainingInstances: Array[Array[Double]],
                                  responses: Array[Int],
                                  lambda: Double = 0.0,
                                  fTask: ArrayBuffer[MultiClassObjectiveFunction.FTask],
                                  gTask: ArrayBuffer[MultiClassObjectiveFunction.GTask],
                                  numClasses: Int /*k*/
                                 ) extends DifferentiableMultivariateFunction {

  def f(w: Array[Double], g: Array[Double]): Double = {
    val p = w.length - 1
    util.Arrays.fill(g, 0.0)
    val value = (gTask.toList match {
      case Nil => Future {
        Double.NaN
      }
      case _ =>
        gTask.map(_.copy(w = w))
        Future {
          gTask.map(_.call)
        }.map {
          _.map { gi =>
            w.indices.foreach(i => g(i) = g(i) + gi(i))
            gi(w.length)
          }.sum
        }.recover {
          case _ =>
            val n = trainingInstances.length
            (0 until n).map { i =>
              val wx = LogisticRegression.dot(trainingInstances(i), w)
              val yi = responses(i) - LogisticRegression.logistic(wx)
              (0 until p).foreach(j => g(i) = g(i) - yi * trainingInstances(i)(j))
              g(p) = g(p) - yi
              LogisticRegression.log1pe(wx) - responses(i) * wx
            }.sum
        }
    }).map { res =>
      val f = if (lambda != 0.0) {
        val w2 = w.map(elem => elem * elem).sum
        (0 until p).foreach(j => g(j) = g(j) + lambda * w(j))
        res + 0.5 * lambda * w2
      } else {
        res
      }
      f
    }
    Await.result(value, 10.seconds)
  }

  override def f(x: Array[Double]): Double = ???
}
//TODO: Yet to complete for multi class function
object MultiClassObjectiveFunction {
  def apply(trainingInstances: Array[Array[Double]], responses: Array[Int], lambda: Double = 0.0, numClasses: Int /*k*/): MultiClassObjectiveFunction = {
    val nProcessors: Int = getThreadPoolSize //m
    val instanceCount: Int = trainingInstances.length //n
    val fTask = new ArrayBuffer[FTask]()
    val gTask = new ArrayBuffer[GTask]()

    if (instanceCount >= 1000 && nProcessors >= 2) {
      val step = if (instanceCount / nProcessors > 100) instanceCount / nProcessors else 100
      val (lStart, _) = (0 until nProcessors).foldLeft((0, step)) {
        case ((start, end), _) =>
          fTask.append(FTask(start, end, trainingInstances, responses))
          gTask.append(GTask(start, end, trainingInstances, responses))
          (start + step, end + step)
      }
      fTask.append(FTask(lStart, instanceCount, trainingInstances, responses))
    }
    new MultiClassObjectiveFunction(trainingInstances, responses, lambda, fTask, gTask, numClasses)
  }

  case class FTask(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], w: Array[Double] = Array.empty[Double]){

    def call: Double = start until end map { i =>
      val wx = LogisticRegression.dot(x(i), w)
      LogisticRegression.log1pe(wx) - y(i) * wx
    } sum
  }

  case class GTask(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], w: Array[Double] = Array.empty[Double]) {

    def call: Array[Double] = {
      val p = w.length - 1
      val g = new Array[Double](w.length + 1)

      val f: immutable.Seq[Double] = (start until end).map { i =>
        val wx = LogisticRegression.dot(x(i), w)
        val yi = y(i) - LogisticRegression.logistic(wx)
        (0 until p).foreach { j => g(j) = g(j) - yi * x(i)(j) }
        g(p) = g(p) - yi
        LogisticRegression.log1pe(wx) - y(i) * wx
      }

      g(p + 2) = f.sum
      g
    }
  }

}

case class FTaskMultiClass(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], w: Array[Double] = Array.empty[Double]){

  def call: Double = start until end map { i =>
    val wx = LogisticRegression.dot(x(i), w)
    LogisticRegression.log1pe(wx) - y(i) * wx
  } sum
}

case class GTaskMultiClass(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], w: Array[Double] = Array.empty[Double]) {

  def call: Array[Double] = {
    val p = w.length - 1
    val g = new Array[Double](w.length + 1)

    val f: immutable.Seq[Double] = (start until end).map { i =>
      val wx = LogisticRegression.dot(x(i), w)
      val yi = y(i) - LogisticRegression.logistic(wx)
      (0 until p).foreach { j => g(j) = g(j) - yi * x(i)(j) }
      g(p) = g(p) - yi
      LogisticRegression.log1pe(wx) - y(i) * wx
    }

    g(p + 2) = f.sum
    g
  }
}
*/

object MulticoreExecutor {

  object IO {
    val CONCURRENCY_FACTOR = 1
    private val numThreads: Int = Try {
      val env = System.getProperty("KSAI.threads")
      env.toInt
    } match {
      case _ => Runtime.getRuntime.availableProcessors()
    }

    implicit lazy val customExecutionContext: concurrent.ExecutionContext = concurrent.ExecutionContext.fromExecutor(
      Executors.newFixedThreadPool(numThreads * CONCURRENCY_FACTOR)
    )

    def getThreadPoolSize: Int = numThreads
  }

}
