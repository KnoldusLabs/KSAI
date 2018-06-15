package ksai.core.classification

import breeze.stats.distributions.Rand
import ksai.kernels.{Kernel, LinearKernel}

import scala.collection.mutable.ArrayBuffer

trait MultiClass

case object ONE_VS_ONE extends MultiClass

case object ONE_VS_ALL extends MultiClass

case class SupportVectorMachine(
                                 kernel: Kernel,
                                 k: Int,
                                 svm: Option[LASVM] = None,
                                 p: Int = 0,
                                 svms: List[LASVM] = Nil,
                                 strategy: MultiClass = ONE_VS_ONE,
                                 wi: List[Double] = Nil,
                                 tol: Double = 1E-3,
                                 TAU: Double = 1E-12
                               ) {
  def learn(x: Array[Array[Double]], y: Int, weight: Double) = {
    if (y < 0 || y >= k) {
      throw new IllegalArgumentException("Invalid label")
    }

    if (weight <= 0.0) {
      throw new IllegalArgumentException("Invalid instance weight: " + weight)
    }

    if (k == 2) {
      if (y == 1) {
        LASVM.process(x, +1, weight)
      } else {
        svm.map(_.process(x, -1, weight))
      }
    } else if (strategy == Multiclass.ONE_VS_ALL) {
      if (wi != null) {
        weight *= wi[y];
      }

      for (int i = 0; i < k; i++) {
        if (y == i) {
          svms.get(i).process(x, +1, weight);
        } else {
          svms.get(i).process(x, -1, weight);
        }
      }
    } else {
      for (int i = 0, m = 0; i < k; i++) {
        for (int j = i + 1; j < k; j++, m++) {
          if (y == i) {
            svms.get(m).process(x, +1, weight);
          } else if (y == j) {
            svms.get(m).process(x, -1, weight);
          }
        }
      }
    }
  }
}

object SupportVectorMachine {

  def apply(kernel: Kernel, C: Double =) = {
    SupportVectorMachine(kernel, C, C)
  }

  def apply(kernel: Kernel, Cp: Double, Cn: Double) = {
    if (Cp < 0.0) {
      throw new IllegalArgumentException("Invalid postive instance soft margin penalty: " + Cp)
    }
    if (Cn < 0.0) {
      throw new IllegalArgumentException("Invalid negative instance soft margin penalty: " + Cn)
    }
    new SupportVectorMachine(kernel, 2, Some(new LASVM(Cp, Cn)))
  }

  def apply(kernel: Kernel, C: Double, k: Int, strategy: MultiClass) = {
    if (C < 0.0) {
      throw new IllegalArgumentException("Invalid soft margin penalty: " + C)
    }

    if (k < 3) {
      throw new IllegalArgumentException("Invalid number of classes: " + k)
    }

    val lasvms = strategy match {
      case ONE_VS_ALL =>
        (0 until k).map(_ => new LASVM(C, C)).toList
      case _ =>

        val svms = ArrayBuffer[LASVM]()
        (0 until k).foreach {
          i =>
            (i + 1 until k).foreach {
              _ => svms += new LASVM(C, C)
            }
        }
        svms.toList
    }
    new SupportVectorMachine(kernel, k, None, 0, lasvms, strategy)
  }

  def apply(kernel: Kernel, C: Double, weight: Array[Double], strategy: MultiClass, tol: Double) = {
    SupportVectorMachine(kernel, C, weight, strategy).copy(tol = tol)
  }

  def apply(kernel: Kernel, C: Double, weight: Array[Double], strategy: MultiClass) = {
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
    val k = weight.length;
    val svms = strategy match {
      case ONE_VS_ALL =>
        (0 until k).map(_ => new LASVM(C, C)).toList
      case _ =>
        val svms = new ArrayBuffer[LASVM]()
        (0 until k).foreach {
          i =>
            (i + 1 until k).foreach {
              j => svms += new LASVM(weight(i) * C, weight(j) * C)
            }
        }
        svms.toList
    }

    new SupportVectorMachine(kernel, k, None, 0, svms, strategy, weight.toList)
  }

}


case class SupportVector(
                          x: Double, // Support Vector
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
    val Q = new Array[Array[Double]](k)(k)
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
      i => if (y(i) > 0) {
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
      i => if (y(i) > 0) {
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

}

//Need to implement

case class LASVM(
                  Cp: Double = 1.0, //The soft margin penalty parameter for positive samples.
                  Cn: Double = 1.0, //The soft margin penalty parameter for negative samples.
                  supportVectors: ArrayBuffer[Option[SupportVector]] = new ArrayBuffer[Option[SupportVector]](),
                  w: ArrayBuffer[Double] = new ArrayBuffer[Double](), //Weight vector for linear SVM.
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
                  svmin: Option[SupportVector] = None,
                  svmax: Option[SupportVector] = None,
                  gmin: Double = Double.MaxValue,
                  gmax: Double = -Double.MaxValue
                ) {

  def predict(x: Array[Double])(implicit kernel: Kernel): Double = {
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

  def predict(x: Double)(implicit kernel: Kernel): Double = {
    var prediction = b
    supportVectors.foreach {
      case Some(sv) =>
        prediction = prediction + (sv.alpha * kernel.k(sv.x, x))
    }
    prediction
  }

}

case object LASVM {

  def learn(supportVectors: Seq[SupportVector], p: Int, kernel: Kernel, x: Array[Double], y: Array[Int],
            weight: Array[Double])(implicit TAU: Double, tol: Double = 1E-3): Unit = {
    var lasvm = LASVM()
    val newP = if (p == 0 && kernel.isInstanceOf[LinearKernel]) {
      if (x.isInstanceOf[Array[Array[Double]]]) {
        val x0: Array[Double] = x(0).asInstanceOf[Array[Double]]
        //        p = x0.length
        x0.length
      } else {
        throw new UnsupportedOperationException("Unsupported data type for linear kernel.")
      }
    }

    var (c1, c2) = supportVectors.foldLeft((0, 0)) {
      case ((c1Elem, c2Elem), svElem: SupportVector) => if (svElem.y > 0) {
        (c1Elem + 1, c2Elem)
      } else if (svElem.y < 0) {
        (c1Elem, c2Elem + 1)
      } else {
        (c1Elem, c2Elem)
      }
    }

    // If the SVM is empty or has very few support vectors, use some
    // instances as initial support vectors.
    val n: Int = x.length
    var i = 0
    while ((c1 < 5 || c2 < 5 || i < n)) {
      if (y(i) == 1 && c1 < 5) {
        if (weight.isEmpty) {
          val (resLasvm, _) = process(x(i), y(i), kernel, lasvm)
          lasvm = resLasvm
        } else {
          val (resLasvm, _) = process(x(i), y(i), kernel, lasvm, weight(i))
          lasvm = resLasvm
        }
        c1 = c1 + 1
      }

      if (y(i) == -1 && c2 < 5) {
        if (weight.isEmpty) {
          val (resLasvm, _) = process(x(i), y(i), kernel, lasvm)
          lasvm = resLasvm
        } else {
          val (resLasvm, _) = process(x(i), y(i), kernel, lasvm, weight(i))
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
        val (resLasvm, _) = process(x(index(i)), y(index(i)), kernel, lasvm)
        lasvm = resLasvm
      } else {
        val (resLasvm, _) = process(x(index(i)), y(index(i)), kernel, lasvm, weight(index(i)))
        lasvm = resLasvm
      }

      do {
        val (reprocessedLASVM, _) = reprocess(tol)(lasvm, kernel, TAU) // at least one call to reprocess
        lasvm = minmax(reprocessedLASVM)
      } while (lasvm.gmax - lasvm.gmin > 1000)
    }
  }

  def reprocess(epsgr: Double)(implicit lasvm: LASVM, kernel: Kernel, TAU: Double = 1E-12) = {
    val (lasvsm, isSMO) = smo(None, None, epsgr)
    (evict(lasvsm), isSMO)
  }

  private def evict(lasvm: LASVM): LASVM = {
    val minMaxLASVM = minmax(lasvm)
    val newSupportVectors = lasvm.supportVectors.map {
      case Some(sv) if sv.alpha == 0 && (sv.gradient >= minMaxLASVM.gmax && 0 >= sv.cmax)
        || (sv.gradient <= minMaxLASVM.gmin && 0 <= sv.cmin) => None
      case anythingElse => anythingElse
    }
    minMaxLASVM.copy(supportVectors = newSupportVectors)
  }


  private def process(x: Double, y: Int, kernel: Kernel, lasvm: LASVM,
                      weight: Double = 1.0)(implicit TAU: Double = 1E-12): (LASVM, Boolean) = {
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
        val (cmin, cmax) = if (y > 0) {
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

  def smo(v1Opt: Option[SupportVector], v2Opt: Option[SupportVector], epsgr: Double)
         (implicit lasvm: LASVM, kernel: Kernel, TAU: Double = 1E-12): (LASVM, Boolean) = {
    // SO working set selection
    // Determine coordinate to process
    val (v1Res, v2Res, mmLASVM) = (v1Opt, v2Opt) match {
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
        lasvm.supportVectors.foldLeft((Some(sv1), Option.empty[SupportVector], lasvm)) {
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
        val (res1, res2) = lasvm.supportVectors.foldLeft((Option.empty[SupportVector], Some(v2))) {
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
        val newSupportVectors = mmLASVM.supportVectors.map {
          case Some(sv) =>
            Some(sv.copy(gradient = step * (v2.kcache(i) - v1.kcache(i))))
          case None =>
            i = i + 1
            None
        }
        val newLASVM = mmLASVM.copy(supportVectors = newSupportVectors, minmaxflag = false)
        // optimality test
        val minMaxedLASVM = minmax(newLASVM)
        val averagedLASVM = minMaxedLASVM.copy(b = (minMaxedLASVM.gmax + minMaxedLASVM.gmin) / 2)
        if (averagedLASVM.gmax - averagedLASVM.gmin < epsgr) {
          (averagedLASVM, false)
        } else {
          (averagedLASVM, true)
        }
      }
      case _ => (mmLASVM, false)
    }

  }

  def minmax(lasvm: LASVM): LASVM = {
    if (!lasvm.minmaxflag) {
      var gmin = Double.MaxValue
      var gmax = -Double.MaxValue
      var svmin: Option[SupportVector] = None
      var svmax: Option[SupportVector] = None

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

  def trainPlattScaling(x: Array[Double], y: Array[Int], lASVM: LASVM)(implicit kernel: Kernel): LASVM = {
    val scores = x.map(xVal => lASVM.predict(xVal))
    lASVM.copy(platt = Some(PlattScaling(scores, y)))
  }

}