package ksai.training.validation

import scala.util.Random

case class CrossValidation(chunks: Int,
                           train: Array[Array[Int]],
                           test: Array[Array[Int]])

object CrossValidation {
  def apply(n: Int, k: Int): CrossValidation = {
    if (n < 0) {
      throw new IllegalArgumentException("Invalid sample size: " + n)
    }
    if (k < 0 || k > n) {
      throw new IllegalArgumentException("Invalid number of CV rounds: " + k)
    }
    val list = (0 until n).toList
    val index = Random.shuffle(list).toArray
    val train: Array[Array[Int]] = new Array[Array[Int]](k)
    val test: Array[Array[Int]] = new Array[Array[Int]](k)
    val chunk = n/k

    0 until k foreach { itr =>
      val end = if (itr == k - 1) n else chunk * (itr + 1)
      val start = chunk * itr
      train(itr) = new Array[Int](n - end + start)
      test(itr) = new Array[Int](end - start)

      (0 until n).foldLeft ((0, 0)) {
        case ((p, q), j) =>
          if (j >= start && j < end) {
            test(itr)(p + 1) = index(j)
            (p + 1, q)
          } else {
            train(itr)(q + 1) = index(j)
            (p, q + 1)
          }
      }
    }
    CrossValidation(k, train, test)
  }
}