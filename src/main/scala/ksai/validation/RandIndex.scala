package ksai.validation

import ksai.util.NumericFunctions

/**
 * Rand Index. Rand index is defined as the number of pairs of objects
 * that are either in the same group or in different groups in both partitions
 * divided by the total number of pairs of objects. The Rand index lies between
 * 0 and 1. When two partitions agree perfectly, the Rand index achieves the
 * maximum value 1. A problem with Rand index is that the expected value of
 * the Rand index between two random partitions is not a constant. This problem
 * is corrected by the adjusted Rand index that assumes the generalized
 * hyper-geometric distribution as the model of randomness. The adjusted Rand
 * index has the maximum value 1, and its expected value is 0 in the case
 * of random clusters. A larger adjusted Rand index means a higher agreement
 * between two partitions. The adjusted Rand index is recommended for measuring
 * agreement even when the partitions compared have different numbers of clusters.
 *
 * @author Haifeng Li
 */
object RandIndex {

    def measure(y1: List[Int], y2: List[Int]): Double = {
        if (y1.length != y2.length) {
            throw new IllegalArgumentException(s"The vector sizes don't match: ${y1.length} != ${y2.length}.")
        }
        // Get # of non-zero classes in each solution
        val n = y1.length
        val label1 = y1.distinct
        val n1 = label1.length
        val label2 = y2.distinct
        val n2 = label2.length

        // Calculate N contingency matrix

    val count = label1.map{ l1 =>
            label2.map{ l2 =>
                    y1.zip(y2).foldLeft(0){
                        case (result, (yv1, yv2)) =>
                            if(yv1 == l1 && yv2 == l2){
                            result + 1
                        } else {
                            result
                        }
                    }
            }
        }

        // Marginals
       val (count1, count2) = count.foldLeft((List[Double](), List[Double]())){
            case ((count1: List[Double], count2), innrCount) =>
                val c1: Double = innrCount.sum.toDouble
                val c2: List[Double] = count2 match {
                    case Nil => innrCount.map(_.toDouble)
                    case _ => count2.zip(innrCount).map{case (resCnt, resinnrCnt) => resCnt + resinnrCnt}
                }
                (count1 :+ c1, c2)
        }

        // Calculate RAND - Non-adjusted
        val rand_T = count.foldLeft(0.0){
            case (result, countInnr) => countInnr.foldLeft(result){
                case (innerResult, nestedCounterInnr) => innerResult + (nestedCounterInnr * nestedCounterInnr)
            }
        } - n

        val rand_P = count1.foldLeft(0.0){
            case (result, cnt) => result + (cnt * cnt)
        } - n

        val rand_Q = count2.foldLeft(0.0){
            case (result, cnt) => result + (cnt * cnt)
        } - n

       (rand_T - 0.5 * rand_P - 0.5 * rand_Q + NumericFunctions.choose(n, 2)) / NumericFunctions.choose(n, 2)
    }

}
