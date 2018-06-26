package ksai.core.classification

import breeze.stats.distributions.Rand
import ksai.kernels.{Kernel, LinearKernel}
import ksai.util.NumericFunctions

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag

trait MultiClass

case object ONE_VS_ONE extends MultiClass

case object ONE_VS_ALL extends MultiClass

case class SupportVectorMachine[A](
                                    kernel: Kernel[A],
                                    k: Int,
                                    svm: Option[LASVM[A]] = None,
                                    p: Int = 0,
                                    svms: List[LASVM[A]] = Nil,
                                    strategy: MultiClass = ONE_VS_ONE,
                                    wi: List[Double] = Nil,
                                    tol: Double = 1E-3,
                                    TAU: Double = 1E-12
                                  ) {
  def learn(x: A, y: Int, weight: Double = 1.0)(implicit kernel: Kernel[A]): SupportVectorMachine[A] = {
    if (y < 0 || y >= k) {
      throw new IllegalArgumentException("Invalid label")
    }

    if (weight <= 0.0) {
      throw new IllegalArgumentException("Invalid instance weight: " + weight)
    }

    if (k == 2) {
      if (y == 1) {
        val resultSVM = svm.map(sv => LASVM.process[A](x, +1, weight)(kernel, sv, TAU))
        this.copy(svm = resultSVM.map { case (vm, _) => vm })
      } else {
        val resultSVM = svm.map(sv => LASVM.process[A](x, -1, weight)(kernel, sv, TAU))
        this.copy(svm = resultSVM.map { case (vm, _) => vm })
      }
    } else {
      strategy match {
        case ONE_VS_ALL =>
          var newWeight = weight
          if (wi.nonEmpty) {
            newWeight = weight * wi(y)
          }
          val resSVMS = svms.toArray
          (0 until k).foreach {
            case kIndex if y == kIndex =>
              val (processedSVMS, _) = LASVM.process[A](x, +1, weight)(kernel, resSVMS(kIndex), TAU)
              resSVMS(kIndex) = processedSVMS
            case kIndex => val (processedSVMS, _) = LASVM.process[A](x, -1, weight)(kernel, resSVMS(kIndex), TAU)
              resSVMS(kIndex) = processedSVMS
          }
          this.copy(svms = resSVMS.toList)
        case _ =>
          val (newSVMS, _) = (0 until k).toList.foldLeft((svms.toArray, 0)) {
            case ((resSVMS, mIndex), index) =>
              (index + 1 until k).foreach {
                case _ if y == index =>
                  val (processedSVMS, _) = LASVM.process[A](x, +1, weight)(kernel, resSVMS(mIndex), TAU)
                  resSVMS(mIndex) = processedSVMS
                case _ => val (processedSVMS, _) = LASVM.process[A](x, -1, weight)(kernel, resSVMS(mIndex), TAU)
                  resSVMS(mIndex) = processedSVMS
              }
              (resSVMS, mIndex + 1)
          }
          this.copy(svms = newSVMS.toList)
      }
    }
  }

  def learn(x: Array[A], y: Array[Int], weight: Array[Double]): Future[SupportVectorMachine[A]] = {
    if (x.length != y.length) {
      throw new IllegalArgumentException((s"The sizes of X and Y don't match: ${x.length} != ${y.length}"))
    }

    if (weight != null && x.length != weight.length) {
      throw new IllegalArgumentException((s"The sizes of X and instance weight don't match: ${x.length} != ${weight.length}"))
    }

    val miny = y.min
    if (miny < 0) {
      throw new IllegalArgumentException("Negative class label:" + miny)
    }

    val maxy = y.max
    if (maxy >= k) {
      throw new IllegalArgumentException("Invalid class label:" + maxy)
    }

    if (k == 2) {
      val yi = new Array[Int](y.length)
      (0 until y.length).foreach {
        case index if y(index) == 1 => y(index) = 1
        case index => y(index) = -1
      }

      if (weight.isEmpty) {
        Future.successful(this.copy(svm = svm.map(vm => LASVM.learn[A](x, yi, Array.empty)(kernel, vm, p, TAU, tol))))
      } else {
        Future.successful(this.copy(svm = svm.map(vm => LASVM.learn[A](x, yi, weight)(kernel, vm, p, TAU, tol))))
      }
    } else {

      strategy match {
        case ONE_VS_ALL =>
          //        List < TrainingTask > tasks = new ArrayList <> (k)

          val tasks = (0 until k).map { index =>

            val yi = new Array[Int](y.length)
            val w = if (wi.isEmpty) weight else new Array[Double](y.length)
            (0 until y.length).foreach { lIndex =>
              if (y(lIndex) == index) {
                yi(lIndex) = +1
              } else {
                yi(lIndex) = -1
              }

              if (wi.nonEmpty) {
                w(lIndex) = wi(y(lIndex))
                if (weight.nonEmpty) {
                  w(lIndex) = w(lIndex) * weight(lIndex)
                }
              }
            }
            val learningPromise = Promise[LASVM[A]]
            TrainingTask(svms(index), x, yi, w).call(learningPromise)(kernel, svms(index), p, TAU, tol)
          }

          Future.sequence(tasks).map(vms => this.copy(svms = vms.toList))

        case _ => {
          var m = 0

          val learningResult: IndexedSeq[Future[LASVM[A]]] = (0 until k).flatMap { index =>
            ((index + 1) until k).map { jIndex =>
              var n = 0
              (0 until y.length).foreach { lIndex =>
                if (y(lIndex) == index || y(lIndex) == jIndex) {
                  n = n + 1
                }
              }

              val xij = Array.fill[A](n) //(T[]) java.lang.reflect.Array.newInstance(x.getClass().getComponentType(), n)

              val yij = new Array[Int](n)
              val wij = if (weight.isEmpty) Array[Double]() else new Array[Double](n)

              var q = 0
              (0 until y.length).foreach { lIndex =>
                if (y(lIndex) == index) {
                  xij(q) = x(lIndex)
                  yij(q) = +1
                  if (weight != null) {
                    wij(q) = weight(lIndex)
                  }
                  q = q + 1
                } else if (y(lIndex) == jIndex) {
                  xij(q) = x(lIndex)
                  yij(q) = -1
                  if (weight != null) {
                    wij(q) = weight(lIndex)
                  }
                  q = q + 1
                }
              }
              val learningPromise = Promise[LASVM[A]]
              val result = new TrainingTask(svms(m), xij, yij, wij).call(learningPromise)(kernel, svms(index), p, TAU, tol)
              m = m + 1
              result
            }
          }
          Future.sequence(learningResult).map(lr => this.copy(svms = lr.toList))
        }
      }
    }
  }

  def finish(): Future[SupportVectorMachine[A]] = {
    if (k == 2) {
      val learningPromise = Promise[LASVM[A]]
      val svmResult = ProcessTask[A](tol, p, svm.getOrElse(throw new IllegalStateException("At this point svm must be present")), kernel, TAU).call(learningPromise)
      svmResult.map {
        s => this.copy(svm = Some(s))
      }
    } else {
      val svmsResult = svms.map { sv =>
        val learningPromise = Promise[LASVM[A]]
        ProcessTask[A](tol, p, sv, kernel, TAU).call(learningPromise)
      }
      Future.sequence(svmsResult).map(lr => this.copy(svms = lr.toList))
    }
  }

  def trainPlattScaling(x: Array[A], y: Array[Int]) = {
    if (k == 2) {
      val plattScalingSVM: Option[LASVM[A]] = svm.map(sv => LASVM.trainPlattScaling[A](x, y, sv)(kernel))
      this.copy(svm = plattScalingSVM)
    } else {
      strategy match {
        case ONE_VS_ALL =>
          //      List<PlattScalingTask> tasks = new util.ArrayList<>(svms.size());
          var m = 0
          val yi = Array[Int](y.length)
          val svmsResult = svms.map { sv =>
            (0 until y.length).foreach {
              index =>
                if (y(index) == m) {
                  yi(index) = 1
                } else {
                  yi(index) = -1
                }
            }
            m = m + 1
            val learningPromise = Promise[LASVM[A]]
            PlattingScalingTask[A](sv, x, y, kernel).call(learningPromise)
          }

          Future.sequence(svmsResult).map {
            svmsRes => this.copy(svms = svmsRes)
          }
        case _ =>
          var m = 0
          val plattingResults = (0 until k).flatMap { index =>
            ((index + 1) until k).map { jIndex =>
              val s = svms(m)
              m = m + 1
              val yi = new Array[Int](y.length)
              (0 until y.length).foreach { pIndex =>
                if (y(pIndex) == index)
                  yi(pIndex) = +1
                else
                  yi(pIndex) = -1
              }
              val learningPromise = Promise[LASVM[A]]
              PlattingScalingTask[A](s, x, yi, kernel).call(learningPromise)
            }
          }

          Future.sequence(plattingResults).map(svs => this.copy(svms = svs.toList))
      }
    }
  }

  def predict(x: A): Int = {
    implicit val kern = kernel
    if (k == 2) {
      svm match {
        case None => throw new IllegalStateException("SVM cannot be none at this stage")
        case Some(vm) if vm.predict(x) > 0 => 1
        case _ => 0
      }
    } else {
      strategy match {
        case ONE_VS_ALL =>
          // one-vs-all
          var label = 0
          var i = 0
          var maxf = Double.NegativeInfinity
          svms.foreach { vm =>
            val f = vm.predict(x)
            if (f > maxf) {
              label = i
              maxf = f
            }
            i = i + 1
          }
          label
        case _ =>
          // one-vs-one
          val count = new Array[Int](k)
          var m = 0
          var max = 0
          var label = 0
          (0 until k).foreach { index =>
            ((index + 1) until k).foreach { jIndex =>
              val f = svms(m).predict(x)(kernel)
              m = m + 1
              if (f > 0) {
                count(index) = count(index) + 1
              } else {
                count(jIndex) = count(jIndex) + 1
              }
            }
          }

          (0 until k).foreach { index =>
            if (count(index) > max) {
              max = count(index)
              label = index
            }
          }
          label
      }
    }
  }

  def predict(x: A, prob: Array[Double]) = {
    implicit val kern = kernel
    if (k == 2) {
      svm match {
        case None =>
          throw new UnsupportedOperationException("PlattScaling was not trained yet. Please call SVM.trainPlattScaling() first.")
        case Some(vm) =>
          // two class
          val y: Double = vm.predict(x)
          prob(1) = posterior(vm, y)
          prob(0) = 1.0 - prob(1)
          if (y > 0) {
            1
          } else {
            0
          }
      }
    } else {
      strategy match {
        case ONE_VS_ALL =>
          // one-vs-all
          var label = 0
          var maxf = Double.NegativeInfinity
          (0 until svms.size).foreach { index =>

            val vm = svms(index)
            vm.platt match {
              case None =>
                throw new UnsupportedOperationException("PlattScaling was not trained yet. Please call SVM.trainPlattScaling() first.")
              case _ =>
                val f = vm.predict(x)
                prob(index) = posterior(vm, f)
                if (f > maxf) {
                  label = index
                  maxf = f
                }
            }
          }
          NumericFunctions.unitize1(prob)
          label

        case _ =>
          // one-vs-one
          val count = new Array[Int](k)
          val r: Array[Array[Double]] = Array.ofDim[Double](k, k) //new Array[Array[Double]](k)(k)
          var m = 0
          (0 until k).foreach { index =>
            ((index + 1) until k).foreach { jIndex =>
              val vm = svms(m)
              vm.platt match {
                case None =>
                  throw new UnsupportedOperationException("PlattScaling was not trained yet. Please call SVM.trainPlattScaling() first.");
                case Some(plat) =>

                  val f = vm.predict(x)
                  r(index)(jIndex) = posterior(vm, f)
                  r(jIndex)(index) = 1.0 - r(index)(jIndex)
                  if (f > 0) {
                    count(index) = count(index) + 1
                  } else {
                    count(jIndex) = count(jIndex) + 1
                  }
              }
              m = m + 1
            }

          }

          PlattScaling.multiclass(k, r, prob)

          var max = 0
          var label = 0
          (0 until k).foreach { index =>
            if (count(index) > max) {
              max = count(index)
              label = index
            }
          }
          label
      }
    }
  }

  def posterior(svm: LASVM[A], y: Double): Double = {
    val minProb = 1e-7
    val maxProb = 1 - minProb

    Math.min(Math.max(svm.platt.getOrElse(throw new IllegalStateException("At this stage PlattingScaling must be present")
    ).predict(y), minProb), maxProb)
  }

}

object SupportVectorMachine {

  def apply[A](kernel: Kernel[A], C: Double): SupportVectorMachine[A] = {
    SupportVectorMachine(kernel, C, C)
  }

  def apply[A](kernel: Kernel[A], Cp: Double, Cn: Double): SupportVectorMachine[A] = {
    if (Cp < 0.0) {
      throw new IllegalArgumentException("Invalid postive instance soft margin penalty: " + Cp)
    }
    if (Cn < 0.0) {
      throw new IllegalArgumentException("Invalid negative instance soft margin penalty: " + Cn)
    }
    new SupportVectorMachine[A](kernel, 2, Some(new LASVM[A](Cp, Cn)))
  }

  def apply[A](kernel: Kernel[A], C: Double, k: Int, strategy: MultiClass): SupportVectorMachine[A] = {
    if (C < 0.0) {
      throw new IllegalArgumentException("Invalid soft margin penalty: " + C)
    }

    if (k < 3) {
      throw new IllegalArgumentException("Invalid number of classes: " + k)
    }

    val lasvms = strategy match {
      case ONE_VS_ALL =>
        (0 until k).map(_ => new LASVM[A](C, C)).toList
      case _ =>

        val svms = ArrayBuffer[LASVM[A]]()
        (0 until k).foreach {
          i =>
            (i + 1 until k).foreach {
              _ => svms += new LASVM[A](C, C)
            }
        }
        svms.toList
    }
    new SupportVectorMachine(kernel, k, None, 0, lasvms, strategy)
  }

  def apply[A](kernel: Kernel[A], C: Double, weight: Array[Double], strategy: MultiClass, tol: Double): SupportVectorMachine[A] = {
    SupportVectorMachine(kernel, C, weight, strategy).copy(tol = tol)
  }

  def apply[A](kernel: Kernel[A], C: Double, weight: Array[Double], strategy: MultiClass): SupportVectorMachine[A] = {
    if (C < 0.0) {
      throw new IllegalArgumentException("Invalid soft margin penalty: " + C)
    }

    if (weight.length < 3) {
      throw new IllegalArgumentException("Invalid number of classes: " + weight.length)
    }

    weight.foreach { wt =>
      if (wt <= 0.0) {
        throw new IllegalArgumentException("Invalid class weight: " + wt)
      }
    }
    val k = weight.length
    val svms = strategy match {
      case ONE_VS_ALL =>
        (0 until k).map(_ => new LASVM[A](C, C)).toList
      case _ =>
        val svms = new ArrayBuffer[LASVM[A]]()
        (0 until k).foreach {
          i =>
            (i + 1 until k).foreach {
              j => svms += new LASVM[A](weight(i) * C, weight(j) * C)
            }
        }
        svms.toList
    }

    new SupportVectorMachine(kernel, k, None, 0, svms, strategy, weight.toList)
  }

}

case class TrainingTask[A](
                            svm: LASVM[A],
                            x: Array[A],
                            y: Array[Int],
                            weight: Array[Double]
                          ) {
  def call(learningPromise: Promise[LASVM[A]])(kernel: Kernel[A], svm: LASVM[A], p: Int = 0,
                                               TAU: Double, tol: Double = 1E-3): Future[LASVM[A]] = {
    Future {
      val result = LASVM.learn[A](x, y, weight)(kernel, svm, p, TAU, tol)
      learningPromise.success(result)
    }
    learningPromise.future
  }
}

case class ProcessTask[A](
                           epsgr: Double,
                           p: Int,
                           svm: LASVM[A],
                           kernel: Kernel[A],
                           TAU: Double
                         ) {
  def call(promise: Promise[LASVM[A]]): Future[LASVM[A]] = {
    Future {
      val finishResult = LASVM.finish[A](epsgr, p)(svm, kernel, TAU) // svm.finish()
      promise.success(finishResult)
    }
    promise.future
  }
}

case class PlattingScalingTask[A](
                                   svm: LASVM[A],
                                   x: Array[A],
                                   y: Array[Int],
                                   kernel: Kernel[A]
                                 ) {

  def call(promise: Promise[LASVM[A]]) = {
    Future {
      val finishResult = LASVM.trainPlattScaling[A](x, y, svm)(kernel)
      promise.success(finishResult)
    }
    promise.future
  }

}


case class SupportVector[A](
                             x: A, // Support Vector
                             y: Int, //Suport vector label
                             alpha: Double, //Lagrangian multiplier of support vector.
                             gradient: Double, //Gradient y - K&alpha.
                             cmin: Double, //Lower bound of alpha.
                             cmax: Double, //Upper bound of alpha.
                             k: Double, //Kernel value k(x, x)
                             kcache: ArrayBuffer[Double] = new ArrayBuffer[Double]() //Kernel value cache.
                           )

case class PlattScaling(alpha: Double, beta: Double) {

  def predict(y: Double) = {
    val fApB = y * alpha + beta

    if (fApB >= 0)
      Math.exp(-fApB) / (1.0 + Math.exp(-fApB))
    else
      1.0 / (1 + Math.exp(fApB))
  }

  def multiclass(k: Int, r: Array[Array[Double]], p: Array[Double]) = {
    val Q = Array.ofDim[Double](k, k)
    val Qp = new Array[Double](k)
    var pQp, eps = 0.005 / k

    (0 until k).foreach { t =>
      p(t) = 1.0 / k // Valid if k = 1
      Q(t)(t) = 0
      (0 until t).foreach { j =>
        Q(t)(t) = Q(t)(t) + r(j)(t) * r(j)(t)
        Q(t)(j) = Q(j)(t)
      }
      (t + 1 until k).foreach { j =>
        Q(t)(t) = Q(t)(t) + r(j)(t) * r(j)(t)
        Q(t)(j) = -r(j)(t) * r(t)(j)
      }
    }

    var iterVal = 0
    val maxIter = Math.max(100, k)
    var isContinue = true
    (iterVal until maxIter).foreach {
      case iter if isContinue =>
        iterVal = iterVal + 1
        // stopping condition, recalculate QP,pQP for numerical accuracy
        pQp = 0.0
        (0 until k).foreach { t =>
          Qp(t) = 0
          (0 until k).foreach {
            j =>
              Qp(t) = Qp(t) + Q(t)(j) * p(j)
          }
          pQp = pQp + p(t) * Qp(t)
        }
        var max_error = 0.0
        (0 until k).foreach { t =>
          val error: Double = Math.abs(Qp(t) - pQp)
          if (error > max_error)
            max_error = error
        }
        if (max_error < eps) {
          isContinue = false
        }

        (0 until k).foreach { t =>
          val diff = (-Qp(t) + pQp) / Q(t)(t)
          p(t) = p(t) + diff
          pQp = (pQp + diff * (diff * Q(t)(t) + 2 * Qp(t))) / (1 + diff) / (1 + diff)
          (0 until k).foreach { j =>
            Qp(j) = (Qp(j) + diff * Q(t)(j)) / (1 + diff)
            p(j) /= (1 + diff)
          }
        }
      case _ => iterVal = iterVal + 1
    }
    p
  }


}

object PlattScaling {

  def apply(scores: Array[Double], y: Array[Int], maxIters: Int = 100): PlattScaling = {
    val l = scores.length
    var prior1 = 0
    var prior0 = 0
    var iVal = 0

    (0 until l).foreach {
      i =>
        if (y(i) > 0) {
          prior1 = prior1 + 1
        } else {
          prior0 = prior0 + 1
        }
        iVal = iVal + 1
    }

    val min_step = 1e-10 // Minimal step taken in line search
    val sigma = 1e-12 // For numerically strict PD of Hessian
    val eps = 1e-5
    val hiTarget = (prior1 + 1.0) / (prior1 + 2.0)
    val loTarget = 1 / (prior0 + 2.0)
    val t: Array[Double] = new Array[Double](l)

    // Initial Point and Initial Fun Value
    var alpha = 0.0
    var beta = Math.log((prior0 + 1.0) / (prior1 + 1.0))
    var fval = 0.0

    (0 until l).foreach {
      i =>
        if (y(i) > 0) {
          t(i) = hiTarget
        } else {
          t(i) = loTarget
        }
        val fApB = scores(i) * alpha + beta
        if (fApB >= 0)
          fval += t(i) * fApB + Math.log(1 + Math.exp(-fApB))
        else
          fval += (t(i) - 1) * fApB + Math.log(1 + Math.exp(fApB))
    }

    var iterVal = 0

    var h11 = sigma // numerically ensures strict PD
    var h22 = sigma
    var h21 = 0.0
    var g1 = 0.0
    var g2 = 0.0
    var isContinue = true
    (0 to maxIters).foreach {
      case iter if (!(Math.abs(g1) < eps && Math.abs(g2) < eps) && isContinue) =>
        iterVal = iterVal + 1
        h11 = sigma // numerically ensures strict PD
        h22 = sigma
        h21 = 0.0
        g1 = 0.0
        g2 = 0.0

        (0 until l).foreach {
          case i =>
            val fApB = scores(i) * alpha + beta
            var p = 0.0
            var q = 0.0
            if (fApB >= 0) {
              p = Math.exp(-fApB) / (1.0 + Math.exp(-fApB))
              q = 1.0 / (1.0 + Math.exp(-fApB))
            } else {
              p = 1.0 / (1.0 + Math.exp(fApB))
              q = Math.exp(fApB) / (1.0 + Math.exp(fApB))
            }
            val d2 = p * q
            h11 = h11 + scores(i) * scores(i) * d2
            h22 = h22 + d2
            h21 = h21 + scores(i) * d2
            val d1 = t(i) - p
            g1 = g1 + scores(i) * d1
            g2 = g2 + d1
        }

        var stepsize = 1.0 // Line Search
        if (!(Math.abs(g1) < eps && Math.abs(g2) < eps)) {
          val det = h11 * h22 - h21 * h21
          val dA = -(h22 * g1 - h21 * g2) / det
          val dB = -(-h21 * g1 + h11 * g2) / det
          val gd = g1 * dA + g2 * dB

          var break = false
          while (stepsize >= min_step && !break) {
            val newA = alpha + stepsize * dA
            val newB = beta + stepsize * dB

            // New function value
            var newf = 0.0
            (0 until l).foreach {
              i =>
                val fApB = scores(i) * newA + newB
                if (fApB >= 0)
                  newf += t(i) * fApB + Math.log(1 + Math.exp(-fApB))
                else
                  newf += (t(i) - 1) * fApB + Math.log(1 + Math.exp(fApB))
            }
            // Check sufficient decrease
            if (newf < fval + 0.0001 * stepsize * gd) {
              alpha = newA
              beta = newB
              fval = newf
              break = true
            } else {
              stepsize = stepsize / 2.0
            }
          }

        }

        if (stepsize < min_step) {
          isContinue = false
        }
      case _ =>
        iterVal = iterVal + 1
    }


    if (iterVal >= maxIters) {
      println("Reaches maximal iterations")
    }

    new PlattScaling(alpha, beta)
  }

  def multiclass(k: Int, r: Array[Array[Double]], p: Array[Double]) = {
    val Q = Array.ofDim[Double](k, k)
    val Qp = new Array[Double](k)
    var pQp, eps = 0.005 / k

    (0 until k).foreach { tIndex =>
      p(tIndex) = 1.0 / k // Valid if k = 1
      Q(tIndex)(tIndex) = 0
      (0 until tIndex).foreach { jIndex =>
        Q(tIndex)(tIndex) = (Q(tIndex)(tIndex)) + (r(jIndex)(tIndex) * r(jIndex)(tIndex))
        Q(tIndex)(jIndex) = Q(jIndex)(tIndex)
      }
      (tIndex + 1 until k).foreach { jIndex =>
        Q(tIndex)(tIndex) = Q(tIndex)(tIndex) + (r(jIndex)(tIndex) * r(jIndex)(tIndex))
        Q(tIndex)(jIndex) = -r(jIndex)(tIndex) * r(tIndex)(jIndex)
      }
    }

    var iter = 0;
    val maxIter = Math.max(100, k)
    var max_error = 100.0
    (0 until maxIter).foreach { case _ if eps < max_error =>
      iter = iter + 1
      // stopping condition, recalculate QP,pQP for numerical accuracy
      pQp = 0
      (0 until k).foreach { tIndex =>
        Qp(tIndex) = 0
        (0 until k).foreach {
          jIndex =>
            Qp(tIndex) = Qp(tIndex) + (Q(tIndex)(jIndex) * p(jIndex))
        }
        pQp = pQp + p(tIndex) * Qp(tIndex)
      }
      max_error = 0.0
      (0 until k).foreach { tIndex =>
        val error = Math.abs(Qp(tIndex) - pQp)
        if (error > max_error)
          max_error = error
      }

      if (max_error > eps) {
        (0 until k).foreach { tIndex =>
          val diff = (-Qp(tIndex) + pQp) / Q(tIndex)(tIndex)
          p(tIndex) = p(tIndex) + diff
          pQp = (pQp + diff * (diff * Q(tIndex)(tIndex) + 2 * Qp(tIndex))) / (1 + diff) / (1 + diff)
          (0 until k).foreach { jIndex =>
            Qp(jIndex) = (Qp(jIndex) + diff * Q(tIndex)(jIndex)) / (1 + diff)
            p(jIndex) = p(jIndex) / (1 + diff)
          }
        }
      }
    }

    if (iter >= maxIter) {
      println("Reaches maximal iterations")
    }
  }

}

//Need to implement

case class LASVM[A](
                     Cp: Double = 1.0, //The soft margin penalty parameter for positive samples.
                     Cn: Double = 1.0, //The soft margin penalty parameter for negative samples.
                     supportVectors: ArrayBuffer[Option[SupportVector[A]]] = new ArrayBuffer[Option[SupportVector[A]]](),
                     w: Array[Double] = new Array[Double](1), //Weight vector for linear SVM.
                     b: Double = 0.0, //Threshold of decision function.
                     nsv: Int = 0, //The number of support vectors.
                     nbsv: Int = 0, //The number of bounded support vectors.
                     platt: Option[PlattScaling] = None, //Platt Scaling for estimating posterior probabilities.
                     minmaxflag: Boolean = false, //If minimax is called after update.
                     /**
                       * Most violating pair.
                       * argmin gi of m_i < alpha_i
                       * argmax gi of alpha_i < M_i
                       * where m_i = min{0, y_i * C}
                       * and   M_i = max{0, y_i * C}
                       */
                     svmin: Option[SupportVector[A]] = None,
                     svmax: Option[SupportVector[A]] = None,
                     gmin: Double = Double.MaxValue,
                     gmax: Double = -Double.MaxValue
                   ) {

  def predict(x: Array[Double])(implicit kernel: Kernel[A]): Double = {
    var prediction = b
    kernel match {
      case lk: LinearKernel =>
        if (w.nonEmpty) {
          (0 until x.length).foreach {
            case i =>
              prediction = prediction + (x(i) * w(i))
          }
        } else {
          throw new UnsupportedOperationException("Unsupported data type for linear kernel")
        }
    }
    prediction
  }

  def predict(x: A)(implicit kernel: Kernel[A]): Double = {
    var prediction = b
    supportVectors.foreach {
      case Some(sv) =>
        prediction = prediction + (sv.alpha * kernel.k(sv.x, x))
    }
    prediction
  }

}

object LASVM {

  def learn[A](x: Array[A], y: Array[Int], weight: Array[Double])(kernel: Kernel[A], svm: LASVM[A], p: Int = 0,
                                                                  TAU: Double, tol: Double = 1E-3): LASVM[A] = {
    var lasvm = svm
    val newP = if (p == 0 && kernel.isInstanceOf[LinearKernel]) {
      if (x.isInstanceOf[Array[Array[Double]]]) {
        val x0: Array[Double] = x(0).asInstanceOf[Array[Double]]
        //        p = x0.length
        x0.length
      } else {
        throw new UnsupportedOperationException("Unsupported data type for linear kernel.")
      }
    }

    var (c1, c2) = lasvm.supportVectors.foldLeft((0, 0)) {
      case ((c1Elem, c2Elem), Some(svElem: SupportVector[A])) => if (svElem.y > 0) {
        (c1Elem + 1, c2Elem)
      } else if (svElem.y < 0) {
        (c1Elem, c2Elem + 1)
      } else {
        (c1Elem, c2Elem)
      }
      case ((c1Elem, c2Elem), _) => (c1Elem, c2Elem)
    }

    // If the SVM is empty or has very few support vectors, use some
    // instances as initial support vectors.
    val n: Int = x.length
    var i = 0
    while ((c1 < 5 || c2 < 5 || i < n)) {
      if (y(i) == 1 && c1 < 5) {
        if (weight.isEmpty) {
          val (resLasvm, _) = process(x(i), y(i))(kernel, lasvm, TAU)
          lasvm = resLasvm
        } else {
          val (resLasvm, _) = process(x(i), y(i), weight(i))(kernel, lasvm, TAU)
          lasvm = resLasvm
        }
        c1 = c1 + 1
      }

      if (y(i) == -1 && c2 < 5) {
        if (weight.isEmpty) {
          val (resLasvm, _) = process(x(i), y(i))(kernel, lasvm, TAU)
          lasvm = resLasvm
        } else {
          val (resLasvm, _) = process(x(i), y(i), weight(i))(kernel, lasvm, TAU)
          lasvm = resLasvm
        }
        c2 = c2 + 1
      }
      i = i + 1
    }

    // train SVM in a stochastic order.
    val index = Rand.permutation(n).get().toArray
    for (i <- 0 until n) {
      if (weight.isEmpty) {
        val (resLasvm, _) = process(x(index(i)), y(index(i)))(kernel, lasvm, TAU)
        lasvm = resLasvm
      } else {
        val (resLasvm, _) = process(x(index(i)), y(index(i)), weight(index(i)))(kernel, lasvm, TAU)
        lasvm = resLasvm
      }

      do {
        val (reprocessedLASVM, _) = reprocess(tol)(lasvm, kernel, TAU) // at least one call to reprocess
        lasvm = minmax(reprocessedLASVM)
      } while (lasvm.gmax - lasvm.gmin > 1000)
    }
    lasvm
  }

  def reprocess[A](epsgr: Double)(implicit lasvm: LASVM[A], kernel: Kernel[A], TAU: Double = 1E-12) = {
    val (lasvsm, isSMO) = smo(None, None, epsgr)
    (evict(lasvsm), isSMO)
  }

  private def evict[A](lasvm: LASVM[A]): LASVM[A] = {
    val minMaxLASVM = minmax(lasvm)
    val newSupportVectors = lasvm.supportVectors.map {
      case Some(sv) if sv.alpha == 0 && (sv.gradient >= minMaxLASVM.gmax && 0 >= sv.cmax)
        || (sv.gradient <= minMaxLASVM.gmin && 0 <= sv.cmin) => None
      case anythingElse => anythingElse
    }
    minMaxLASVM.copy(supportVectors = newSupportVectors)
  }

  def process[A](x: A, y: Int, weight: Double = 1.0)(implicit kernel: Kernel[A], lasvm: LASVM[A], TAU: Double = 1E-12): (LASVM[A], Boolean) = {
    if (y != +1 && y != -1) {
      throw new IllegalArgumentException("Invalid label: " + y)
    }

    if (weight <= 0.0) {
      throw new IllegalArgumentException("Invalid instance weight: " + weight)
    }

    if (lasvm.supportVectors.find(_.map(_.x == x).fold(false)(identity)).isDefined) {
      (lasvm, true)
    } else {
      // Compute gradient
      var gradient: Double = y
      val kcache = new ArrayBuffer[Double]() //DoubleArrayList(sv.size() + 1)
      val (minMaxLavsm, resultBoolean) = if (lasvm.supportVectors.nonEmpty) {
        lasvm.supportVectors.foreach {
          case Some(sv) =>
            // Bail out if already in expansion?
            val k = kernel.k(sv.x, x)
            gradient = gradient - (sv.alpha * k)
            kcache :+ k
          case _ => kcache :+ (0.0)
        }

        // Decide insertion
        val newLavsm = minmax(lasvm)
        if (newLavsm.gmin < newLavsm.gmax) {
          if ((y > 0 && gradient < newLavsm.gmin) || (y < 0 && gradient > newLavsm.gmax)) {
            (newLavsm, false)
          } else {
            (newLavsm, true)
          }
        } else {
          (newLavsm, true)
        }
      } else {
        (lasvm, true)
      }

      if (!resultBoolean) {
        (minMaxLavsm, resultBoolean)
      } else {
        val (cmin: Double, cmax: Double) = if (y > 0) {
          (0, weight * minMaxLavsm.Cp)
        } else {
          (-weight * minMaxLavsm.Cn, 0)
        }
        val sv = new SupportVector(x, y, 0.0, gradient, cmin, cmax, kernel.k(x, x), kcache)
        val i = minMaxLavsm.supportVectors.length
        for (j <- 0 until minMaxLavsm.supportVectors.length) {
          minMaxLavsm.supportVectors.map(_.map(_.kcache :+ kcache(j)))
        }
        var j = 0
        minMaxLavsm.supportVectors.foreach {
          case Some(v) => v.kcache :+ kcache(j)
            j = j + 1
          case _ => j = j + 1
        }
        sv.kcache += sv.k
        minMaxLavsm.supportVectors :+ Some(sv)

        val (smoLASVM, _) = if (y > 0) {
          smo(None, Some(sv), 0.0)(minMaxLavsm, kernel, TAU)
        } else {
          smo(Some(sv), None, 0.0)
        }
        (smoLASVM.copy(minmaxflag = false), true)
      }
    }


  }

  def smo[A](v1Opt: Option[SupportVector[A]], v2Opt: Option[SupportVector[A]], epsgr: Double)
            (implicit lasvm: LASVM[A], kernel: Kernel[A], TAU: Double = 1E-12): (LASVM[A], Boolean) = {
    // SO working set selection
    // Determine coordinate to process
    val (v1Res, v2Res, mmLASVM: LASVM[A]) = (v1Opt, v2Opt) match {
      case (None, None) => {
        val minMaxLasvm = minmax(lasvm)

        if (minMaxLasvm.gmax > -minMaxLasvm.gmin) {
          (v1Opt, minMaxLasvm.svmax, minMaxLasvm)
        } else {
          (minMaxLasvm.svmin, v2Opt, minMaxLasvm)
        }
      }

      case (Some(sv1), None) => {
        if (sv1.kcache.isEmpty) {
          lasvm.supportVectors.foreach {
            case Some(sv) => {
              sv1.kcache :+ (kernel.k(sv1.x, sv.x))
            }
            case None => {
              sv1.kcache :+ (0.0)
            }
          }
        }

        // determine imax
        val km = sv1.k
        val gm = sv1.gradient
        var best = 0.0

        var i: Int = 0
        lasvm.supportVectors.foldLeft((Some(sv1), Option.empty[SupportVector[A]], lasvm)) {
          case ((resV1, resV2, lvm), None) =>
            i = i + 1
            (resV1, resV2, lvm)
          case ((resV1, resV2, lvm), Some(lasv)) =>
            val Z: Double = lasv.gradient - gm
            val k: Double = sv1.kcache(i)
            var curv: Double = km + lasv.k - 2.0 * k
            if (curv <= 0.0) curv = TAU
            val mu: Double = Z / curv
            var resSV = resV2
            if ((mu > 0.0 && lasv.alpha < lasv.cmax) || (mu < 0.0 && lasv.alpha > lasv.cmin)) {
              val gain = Z * mu
              if (gain > best) {
                best = gain
                resSV = Some(lasv)
              }
            }
            i = i + 1
            (resV1, resSV, lvm)
        }
      }

      case (None, Some(v2)) => {
        if (v2.kcache.isEmpty) {
          //          v2.kcache = new DoubleArrayList(sv.size())
          lasvm.supportVectors.foreach {
            case Some(sv) => {
              v2.kcache :+ (kernel.k(v2.x, sv.x))
            }
            case None => {
              v2.kcache :+ (0.0)
            }
          }
        }

        // determine imin
        val km: Double = v2.k
        val gm: Double = v2.gradient
        var best = 0.0
        var i = 0
        val (res1, res2) = lasvm.supportVectors.foldLeft((Option.empty[SupportVector[A]], Some(v2))) {
          case ((resV1, resV2), None) =>
            i = i + 1
            (resV1, resV2)
          case ((resV1, resV2), Some(sv)) =>
            val Z = gm - sv.gradient
            val k = v2.kcache(i)
            var curv = km + sv.k - 2.0 * k
            if (curv <= 0.0) curv = TAU

            val mu = Z / curv
            var resV = resV1
            if ((mu > 0.0 && sv.alpha > sv.cmin) || (mu < 0.0 && sv.alpha < sv.cmax)) {
              val gain = Z * mu
              if (gain > best) {
                best = gain
                resV = Some(sv)
              }
            }
            (resV1, resV2)
        }
        (res1, res2, lasvm)
      }
    }

    (v1Res, v2Res) match {
      case (Some(v1), Some(v2)) => {
        if (v1.kcache.isEmpty) {
          mmLASVM.supportVectors.foreach {
            case Some(sv) => {
              v1.kcache :+ (kernel.k(v1.x, sv.x))
              v2.kcache :+ (kernel.k(v1.x, sv.x))
            }
            case None => {
              v1.kcache :+ (0.0)
              v2.kcache :+ (0.0)
            }
          }
        }

        // Determine curvature
        var curv = v1.k + v2.k - 2 * kernel.k(v1.x, v2.x)
        if (curv <= 0.0) curv = TAU
        var step = (v2.gradient - v1.gradient) / curv

        // Determine maximal step
        if (step >= 0.0) {
          var ostep: Double = v1.alpha - v1.cmin
          if (ostep < step) {
            step = ostep
          }
          ostep = v2.cmax - v2.alpha
          if (ostep < step) {
            step = ostep
          }
        } else {
          var ostep = v2.cmin - v2.alpha
          if (ostep > step) {
            step = ostep
          }
          ostep = v1.alpha - v1.cmax
          if (ostep > step) {
            step = ostep
          }
        }
        // Perform update
        val newv1 = v1.copy(alpha = v1.alpha - step)
        val newv2 = v2.copy(alpha = v1.alpha + step)
        var i = 0
        val newSupportVectors: ArrayBuffer[Option[SupportVector[A]]] = mmLASVM.supportVectors.map {
          case None =>
            i = i + 1
            Option.empty[SupportVector[A]]
          case Some(sv: SupportVector[A]) =>
            val optSV = sv.copy[A](gradient = (step * (v2.kcache(i) - v1.kcache(i))))
            i = i + 1
            Some(optSV)
        }

        val newLASVM = mmLASVM.copy[A](supportVectors = newSupportVectors, minmaxflag = false)
        // optimality test
        val minMaxedLASVM = minmax(newLASVM)
        val averagedLASVM = minMaxedLASVM.copy[A](b = (minMaxedLASVM.gmax + minMaxedLASVM.gmin) / 2)
        if (averagedLASVM.gmax - averagedLASVM.gmin < epsgr) {
          (averagedLASVM, false)
        } else {
          (averagedLASVM, true)
        }
      }
      case _ => (mmLASVM, false)
    }

  }

  def minmax[A](lasvm: LASVM[A]): LASVM[A] = {
    if (!lasvm.minmaxflag) {
      var gmin = Double.MaxValue
      var gmax = -Double.MaxValue
      var svmin: Option[SupportVector[A]] = None
      var svmax: Option[SupportVector[A]] = None

      lasvm.supportVectors.foreach {
        case Some(v) => {
          if (v.gradient < gmin && v.alpha > v.cmin) {
            gmin = v.gradient
            svmin = Some(v)
          }
          if (v.gradient > gmax && v.alpha < v.cmax) {
            gmax = v.gradient
            svmax = Some(v)
          }
        }
        case _ => //Do nothing
      }
      lasvm.copy(svmin = svmin, svmax = svmax, minmaxflag = true)
    } else {
      lasvm
    }
  }

  def trainPlattScaling[A](x: Array[A], y: Array[Int], lASVM: LASVM[A])(implicit kernel: Kernel[A]): LASVM[A] = {
    val scores = x.map(xVal => lASVM.predict(xVal))
    lASVM.copy(platt = Some(PlattScaling(scores, y)))
  }

  def finish[A](epsgr: Double, p: Int = 0)(implicit lasvm: LASVM[A], kernel: Kernel[A], TAU: Double = 1E-12): LASVM[A] = {
    val trimmedSupportVectors: ArrayBuffer[Option[SupportVector[A]]] = lasvm.supportVectors.map {
      (sv: Option[SupportVector[A]]) =>
        sv.flatMap {
          case v if (v.alpha == 0 && ((v.gradient >= lasvm.gmax && 0 >= v.cmax) || (v.gradient <= lasvm.gmin && 0 <= v.cmin))) =>
            None
          case other => Some(other)
        }
    }
    val cleanedUpLASVM = cleanup()(lasvm.copy[A](supportVectors = trimmedSupportVectors), kernel, TAU)
    var newP = p
    kernel match {
      case lk: LinearKernel =>
        val flattenSV = cleanedUpLASVM.supportVectors.flatten
        if (p == 0 && flattenSV.nonEmpty) {
          flattenSV(0).x match {
            case xDouble if xDouble.isInstanceOf[Array[Double]] =>
              newP = xDouble.asInstanceOf[Array[Double]].length
            case xFloat if xFloat.isInstanceOf[Array[Float]] =>
              newP = xFloat.asInstanceOf[Array[Float]].length
            case _ => throw new UnsupportedOperationException("Unsupported data type for linear kernel.")
          }
        }
    }
    val newW = new Array[Double](newP)
    cleanedUpLASVM.supportVectors.foreach { case Some(v) =>
      v.x match {
        case xDouble: Array[Double] =>
          var xI = 0
          xDouble.foreach { xD =>
            newW(xI) += (xD * v.alpha)
            xI = xI + 1
          }
        case xInt: Array[Int] =>
          var xI = 0
          xInt.foreach { xD =>
            newW(xInt(xI)) = v.alpha
            xI = xI + 1
          }
        //          case xInt: Array[Int] => For SparseArray
      }
    case _ => //Do nothing
    }

    cleanedUpLASVM.copy[A](w = newW) //new double[p];
  }

  def cleanup[A]()(implicit lasvm: LASVM[A], kernel: Kernel[A], TAU: Double = 1E-12) = {
    var nsv = 0;
    var nbsv = 0;
    val newSV: ArrayBuffer[Option[SupportVector[A]]] = lasvm.supportVectors.map { case vOpt: Option[SupportVector[A]] =>
      vOpt.map { case sv: SupportVector[A] =>
        nsv = nsv + 1
        if (sv.alpha == sv.cmin || sv.alpha == sv.cmax) {
          nbsv = nbsv + 1
        }
        sv.copy[A](kcache = new ArrayBuffer[Double]())
      }
    }

    lasvm.copy(nsv = nsv, nbsv = nbsv, supportVectors = newSV)
  }

}
