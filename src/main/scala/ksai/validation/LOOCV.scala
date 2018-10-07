package ksai.validation

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

//Leave-one-out cross validation
case class LOOCV(
                train: ArrayBuffer[ArrayBuffer[Int]],
                test: ArrayBuffer[Int]
                )

object LOOCV{

  def apply(n: Int): LOOCV = {
    if (n < 0) {
      throw new IllegalArgumentException("Invalid sample size: " + n)
    }

    val train =  ArrayBuffer[ArrayBuffer[Int]]((0 to n-1).map(_ => ArrayBuffer((0 to n-2).map(_ => 0):_*)):_*)
    val test = ArrayBuffer[Int]((0 to n-1).map(_ => 0):_*)

    (0 to n-1).foreach{ index =>
      test(index) = index
      var p = 0
      (0 to n-1).foreach { jIndex =>
        if (jIndex != index) {
          train(index)(p) = jIndex
          p = p+1
        }
      }
    }
    new LOOCV(train, test)
  }

  def slice(data: ArrayBuffer[ArrayBuffer[Double]], indices: ArrayBuffer[Int]): ArrayBuffer[ArrayBuffer[Double]] = {
    val n = indices.length
    val x = new ArrayBuffer[ArrayBuffer[Double]]()

    (0 to n-1).foreach{ index =>
      x += data(indices(index))
    }
    x
  }

  //TODO: Make them generic to do good
  def sliceY(data: ArrayBuffer[Int], indices: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    val n = indices.length
    val x = new ArrayBuffer[Int]()

    (0 to n-1).foreach{ index =>
      x += data(indices(index))
    }
    x
  }

  /**Generic but need to convert to required collection explicitly as it returns an ArrayMap*/
  def slice[T](data: Array[T], indices: Array[Int]): mutable.ArraySeq[T] = {
    indices.map { itr =>
      data(itr)
    }
  }

}