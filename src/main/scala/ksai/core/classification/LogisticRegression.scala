package ksai.core.classification

import java.util

import ksai.math.DifferentiableMultivariateFunction
import ksai.util.MulticoreExecutor.IO._
import ksai.util.NumericFunctions

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

/**
  * Logistic Regression
  * A generalized
  * linear model used for binomial regression.
  */
trait LogisticRegression extends SoftClassifier[Array[Double]]{

  /** The dimension of input space. */
  def dimensions: Int

  /** Different labels for output space */
  def labels: Array[Int]

  /** Regularization factor. &lambda; > 0 gives a "regularized" estimate
    * of linear weights which often has superior generalization
    * performance, especially when the dimensionality is high.
    */
  def lambda: Double

  /** the tolerance for stopping iterations.*/
  def tolerance: Double

  /** The maximum number of BFGS iterations. */
  def maxIterations: Int

  /** The log-likelihood of learned model */
  def logLikelihood: Double

  /** The linear weights for binary logistic regression. */
  def linearWeights: Array[Double] //w

  /** The linear weights for multi-class logistic regression. */
  def linearWeightsMultiClass: Array[Array[Double]]

  /**
    * Predicts the class label of an instance.
    *
    * @param x the instance to be classified.
    * @return the predicted class label.
    */
  def predict(x: Array[Double]): Int

  /**
    * Predicts the class label of an instance and also calculate a posteriori
    * probabilities. Classifiers may NOT support this method since not all
    * classification algorithms are able to calculate such a posteriori
    * probabilities.
    *
    * @param x          the instance to be classified.
    * @param posterior the array to store a posteriori probabilities on output.
    * @return the predicted class label
    */
  def predict(x: Array[Double], posterior: Array[Double]): Int
}

object LogisticRegression {

  private case class LogisticRegressionImpl(dimensions: Int,
                                            labels: Array[Int],
                                            lambda: Double = 0.0,
                                            tolerance: Double = 1E-5,
                                            maxIterations: Int = 500,
                                            logLikelihood: Double = 0.0,
                                            linearWeights: Array[Double] = Array.empty[Double], //w
                                            linearWeightsMultiClass: Array[Array[Double]] = Array.empty[Array[Double]] //W
                                           ) extends LogisticRegression {

    private val labelCount: Int = labels.length //k
    private val inputLength: Int = dimensions //p

    def predict(x: Array[Double]): Int = predict(x, Array.empty[Double])

    def predict(x: Array[Double], posterior: Array[Double]): Int = {
      if (x.length != inputLength) {
        throw new IllegalArgumentException(s"Invalid input vector size: ${x.length}, expected: $inputLength")
      }

      if (!(posterior sameElements Array.empty[Double]) && posterior.length != labelCount) {
        throw new IllegalArgumentException(s"Invalid posterior vector size: ${posterior.length}, expected: $labelCount")
      }
      if (labelCount == 2) {
        val f = 1.0 / (1.0 + Math.exp(-dot(x, linearWeights)))
        if (!(posterior sameElements Array.empty[Double])) {
          posterior(0) = 1.0 - f
          posterior(1) = f
        }
        if (f < 0.5) {
          0
        } else {
          1
        }
      } else {
        val l = -1
        val (label, max) = (0 until labelCount).foldLeft((l, Double.NegativeInfinity)) {
          case ((res, tempMax), i) =>
            val prob = dot(x, linearWeightsMultiClass(i))
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
          val Z = (0 until labelCount).foldLeft(0.0) { (z, i) =>
            posterior(i) = Math.exp(posterior(i) - max)
            z + posterior(i)
          }

          (0 until labelCount).foreach { i =>
            posterior(i) = posterior(i) / Z
          }
        }
        label
      }
    }

  }

  def apply(trainingInstances: Array[Array[Double]],
            responses: Array[Int],
            lambda: Double = 0.0,
            tolerance: Double = 1E-5,
            maxIterations: Int = 500): LogisticRegression = {

    require(trainingInstances.nonEmpty, "input data must have something to train")
    require(trainingInstances.length == responses.length, s"The sizes of X and Y don't match: ${trainingInstances.length} != ${responses.length}")
    require(lambda >= 0.0, "Invalid regularization factor: " + lambda)
    require(tolerance > 0.0, "Invalid tolerance: " + tolerance)
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

    if (labels.length == 2) {
      val func = BinaryObjectiveFunction(trainingInstances, responses, lambda)
      val p = trainingInstances.head.length
      val linearWeights = new Array[Double](p + 1)

      val logLikelihood = try {
        NumericFunctions.min(func, 5, linearWeights, tolerance, maxIterations)
      } catch {
        case _: Throwable => NumericFunctions.min(func, linearWeights, tolerance, maxIterations)
      }
      LogisticRegressionImpl(dimensions = p, labels, lambda, tolerance, maxIterations, logLikelihood, linearWeights)
    } else {
      val p = trainingInstances.head.length
      val func = MultiClassObjectiveFunction(trainingInstances, responses, lambda, labels.length)
      val w = new Array[Double](labels.length * (p + 1))

      val logLikelihood = try {
        NumericFunctions.min(func, 5, w, tolerance, maxIterations)
      } catch {
        case _: Throwable => NumericFunctions.min(func, w, tolerance, maxIterations)
      }

      val W = new Array[Array[Double]](labels.length).map(_ => Array.fill(p + 1)(0.0))


      val k = labels.length

      (0 until k).foreach{i =>
        W(i) = w.slice(i * (p + 1), (i * (p + 1)) + (p + 1))
      }

      LogisticRegressionImpl(dimensions = p, labels, lambda, tolerance, maxIterations, logLikelihood, linearWeightsMultiClass = W)
    }
  }

  /**
    * Calculate softMax function without overflow.
    */
  def softMax(prob: Array[Double]): Unit = {
    val max = prob.indices.foldLeft(Double.NegativeInfinity) {
      case (tempMax, itr) if prob(itr) > tempMax => prob(itr)
      case (tempMax, _) => tempMax
    }

    val Z = prob.indices.foldLeft(0.0) { (z, itr) =>
      val p = Math.exp(prob(itr) - max)
      prob(itr) = p
      z + p
    }

    prob.indices.foreach(i => prob(i) = prob(i) / Z)
  }

  /**
    * Returns the dot product between weight vector and x (augmented with 1).
    */
  def dot(x: Array[Double], w: Array[Double]): Double = {
    val indices = x.indices
    val (sum, i) = indices.foldLeft((0.0, 0)) {
      case ((tempSum, _), itr) => (tempSum + x(itr) * w(itr), itr + 1)
    }
    sum + w(i)
  }

  /**
    * Returns the dot product between weight vector and x (augmented with 1).
    */
  def dot(x: Array[Double], w: Array[Double], pos: Int): Double = {
    val indices = x.indices
    indices.map(i => x(i) * w(pos + i)).sum + w(pos + x.length)
  }

  /**
    * Returns natural log(1+exp(x)) without overflow.
    */
  def log1pe(x: Double): Double = {
    val y = 0.0
    if (x > 15) x
    else y + Math.log1p(Math.exp(x))
  }

  /**
    *
    * @param trainingInstances Training instances.
    * @param responses Training labels.
    * @param lambda Regularization factor
    * @param fTask Parallel computing of objective function.
    * @param gTask Parallel computing of objective function and gradient.
    */
  class BinaryObjectiveFunction(trainingInstances: Array[Array[Double]],
                                responses: Array[Int],
                                lambda: Double = 0.0,
                                fTask: ArrayBuffer[LogisticRegression.BinaryObjectiveFunction.FTask],
                                gTask: ArrayBuffer[LogisticRegression.BinaryObjectiveFunction.GTask]
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
            case ex =>
              println(s"Failed to train Logistic Regression on multi -core $ex")
              Double.NaN
          }
      }).map { res =>

        val tempF = if (res.equals(Double.NaN)) {
          val n = trainingInstances.length
          (0 until n).foldLeft(0.0) { (sum, i) =>
            val wx = LogisticRegression.dot(trainingInstances(i), w)
            val yi = responses(i) - NumericFunctions.logistic(wx)
            (0 until p).foreach(j => g(j) = g(j) - yi * trainingInstances(i)(j))
            g(p) = g(p) - yi
            sum + LogisticRegression.log1pe(wx) - responses(i) * wx
          }
        } else {
          res
        }

        val f = if (lambda != 0.0) {
          val w2 = (0 until p).foldLeft(0.0) { (sum, i) => sum + w(i) * w(i) }
          (0 until p).foreach(j => g(j) = g(j) + lambda * w(j))
          tempF + 0.5 * lambda * w2
        } else {
          tempF
        }
        f
      }
      Await.result(value, Duration.Inf)
    }

    def f(w: Array[Double]): Double = {
      val p = w.length - 1

      val value = (fTask.toList match {
        case Nil => Future {
          Double.NaN
        }
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

        val tempF = if (res.equals(Double.NaN)) {
          val n = trainingInstances.length
          (0 until n).foldLeft(0.0) { (sum, i) =>
            val wx = LogisticRegression.dot(trainingInstances(i), w)
            sum + LogisticRegression.log1pe(wx) - responses(i) * wx
          }
        } else {
          res
        }

        val f = if (lambda != 0.0) {
          val w2 = (0 until p).foldLeft(0.0) { (sum, i) => sum + w(i) * w(i) }
          tempF + 0.5 * lambda * w2
        } else {
          tempF
        }
        f
      }
      Await.result(value, Duration.Inf)
    }
  }

  object BinaryObjectiveFunction {

    def apply(trainingInstances: Array[Array[Double]], responses: Array[Int], lambda: Double = 0.0): BinaryObjectiveFunction = {
      val nProcessors: Int = getThreadPoolSize //m
      val instanceCount: Int = trainingInstances.length //n
      val fTask = new ArrayBuffer[LogisticRegression.BinaryObjectiveFunction.FTask]()
      val gTask = new ArrayBuffer[LogisticRegression.BinaryObjectiveFunction.GTask]()

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

    case class FTask(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], w: Array[Double] = Array.empty[Double]) {

      def call: Double = start until end map { i =>
        val wx = LogisticRegression.dot(x(i), w)
        LogisticRegression.log1pe(wx) - y(i) * wx
      } sum
    }

    case class GTask(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], w: Array[Double] = Array.empty[Double]) {

      def call: Array[Double] = {
        val p = w.length - 1
        val g = new Array[Double](w.length + 1)

        val f = (start until end).map { i =>
          val wx = LogisticRegression.dot(x(i), w)
          val yi = y(i) - NumericFunctions.logistic(wx)
          (0 until p).foreach { j => g(j) = g(j) - yi * x(i)(j) }
          g(p) = g(p) - yi
          LogisticRegression.log1pe(wx) - y(i) * wx
        }

        g(w.length) = f.sum
        g
      }
    }

  }

  /**
    *
    * @param trainingInstances Training instances.
    * @param responses Training labels.
    * @param lambda Regularization factor
    * @param fTask Parallel computing of objective function.
    * @param gTask Parallel computing of objective function and gradient.
    * @param numClasses The number of classes.
    */
  class MultiClassObjectiveFunction(trainingInstances: Array[Array[Double]],
                                    responses: Array[Int],
                                    lambda: Double = 0.0,
                                    fTask: ArrayBuffer[MultiClassObjectiveFunction.FTask],
                                    gTask: ArrayBuffer[MultiClassObjectiveFunction.GTask],
                                    numClasses: Int //k
                                   ) extends DifferentiableMultivariateFunction {

    def f(w: Array[Double], g: Array[Double]): Double = {
      val p = trainingInstances(0).length
      val prob = new Array[Double](numClasses)

      util.Arrays.fill(g, 0.0)
      val value = (gTask.toList match {
        case Nil => Future {
          Double.NaN
        }
        case _ =>
          Future {
            gTask.map(_.copy(w = w)).map(_.call)
          }.map {
            _.map { gi =>
              w.indices.foreach(i => g(i) = g(i) + gi(i))
              gi(w.length)
            }.sum
          }.recover {
            case ex =>
              ex.printStackTrace()
              println(s"Failed to train Logistic Regression on multi -core $ex")
              Double.NaN
          }
      }).map { res =>

        val tempF = if (res.equals(Double.NaN)) {
          val n = trainingInstances.length
          (0 until n).foldLeft(0.0) { (sum, i) =>
            (0 until numClasses).foreach(j =>
              prob(j) = LogisticRegression.dot(trainingInstances(i), w, j * (p + 1))
            )
            LogisticRegression.softMax(prob)

            (0 until numClasses).foreach { j =>
              val yi = (if (responses(i) == j) 1.0 else 0.0) - prob(j)

              val pos = j * (p + 1)
              (0 until p).foreach(l => g(pos + l) = g(pos + l) - yi * trainingInstances(i)(l))
              g(j * (p + 1) + p) = g(j * (p + 1) + p) - yi
            }

            sum - NumericFunctions.log(prob(responses(i)))
          }
        } else {
          res
        }

        val f = if (lambda != 0.0) {

          val w2 = (0 until numClasses).map { i =>
            (0 until p).map { j =>
              val pos = i * (p + 1) + j
              g(pos) = g(pos) + lambda * w(pos)
              w(pos) * w(pos)
            }.sum
          }.sum

          tempF + 0.5 * lambda * w2
        } else {
          tempF
        }
        f
      }

      Await.result(value, Duration.Inf)
    }

    override def f(w: Array[Double]): Double = {
      val p = trainingInstances.head.length
      val prob = new Array[Double](numClasses)
      val value = (fTask.toList match {
        case Nil => Future {
          Double.NaN
        }
        case _ =>
          Future {
            fTask.map(_.copy(w = w)).map(_.call)
          }.map {
            _.sum
          }.recover {
            case ex =>
              println(s"Failed to train Logistic Regression on multi -core $ex")
              Double.NaN
          }
      }).map { res =>

        val tempF = if (res.equals(Double.NaN)) {
          val n = trainingInstances.length
          (0 until n).foldLeft(0.0) { (sum, i) =>
            (0 until numClasses).foreach(j =>
              prob(j) = LogisticRegression.dot(trainingInstances(i), w, j * (p + 1))
            )
            LogisticRegression.softMax(prob)
            sum - NumericFunctions.log(prob(responses(i)))
          }
        } else {
          res
        }

        val f = if (lambda != 0.0) {
          val w2 = (0 until numClasses).foldLeft(0.0) { (sum, i) =>
            sum + (0 until p).map(j => NumericFunctions.sqr(w(i * (p + 1) + j))).sum
          }
          tempF + 0.5 * lambda * w2
        } else {
          tempF
        }
        f
      }

      Await.result(value, Duration.Inf)
    }
  }

  object MultiClassObjectiveFunction {

    def apply(trainingInstances: Array[Array[Double]],
              responses: Array[Int],
              lambda: Double = 0.0,
              numClasses: Int /*k*/): MultiClassObjectiveFunction = {
      val nProcessors: Int = getThreadPoolSize //m
      val instanceCount: Int = trainingInstances.length //n
      val fTask = new ArrayBuffer[LogisticRegression.MultiClassObjectiveFunction.FTask]()
      val gTask = new ArrayBuffer[LogisticRegression.MultiClassObjectiveFunction.GTask]()

      if (instanceCount >= 1000 && nProcessors >= 2) {
        val step = if (instanceCount / nProcessors > 100) instanceCount / nProcessors else 100
        val (lStart, _) = (0 until nProcessors).foldLeft((0, step)) {
          case ((start, end), _) =>
            fTask.append(FTask(start, end, trainingInstances, responses, numClasses))
            gTask.append(GTask(start, end, trainingInstances, responses, numClasses))
            (start + step, end + step)
        }
        fTask.append(FTask(lStart, instanceCount, trainingInstances, responses, numClasses))
      }
      new MultiClassObjectiveFunction(trainingInstances, responses, lambda, fTask, gTask, numClasses)
    }

    case class FTask(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], numClasses: Int /*k*/ , w: Array[Double] = Array.empty[Double]) {

      def call: Double = {
        val p = x.head.length
        val prob = new Array[Double](numClasses)
        (start until end).foldLeft(0.0) { (s, i) =>
          0 until numClasses foreach (j => prob(j) = LogisticRegression.dot(x(i), w, j * (p + 1)))
          LogisticRegression.softMax(prob)
          s - NumericFunctions.log(prob(y(i)))
        }
      }
    }

    case class GTask(start: Int, end: Int, x: Array[Array[Double]], y: Array[Int], numClasses: Int /*k*/ , w: Array[Double] = Array.empty[Double]) {

      def call: Array[Double] = {
        val p = x.head.length
        val g = new Array[Double](w.length + 1)

        val prob = new Array[Double](numClasses)

        val f = (start until end).foldLeft(0.0) { (s, i) =>
          (0 until numClasses).foreach(j =>
            prob(j) = LogisticRegression.dot(x(i), w, j * (p + 1))
          )
          LogisticRegression.softMax(prob)

          (0 until numClasses).foreach { j =>
            val yi = (if (y(i) == j) 1.0 else 0.0) - prob(j)
            val pos = j * (p + 1)
            (0 until p).foreach(l => g(pos + l) = g(pos + l) - yi * x(i)(l))
            g(j * (p + 1) + p) = g(j * (p + 1) + p) - yi
          }

          s - NumericFunctions.log(prob(y(i)))
        }

        g(w.length) = f
        g
      }
    }

  }

}
