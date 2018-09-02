package ksai.util

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.collection.immutable

trait BreezeImplicits {

  implicit class DenseMatrixOps[A](denseMatrix: DenseMatrix[A]) {
    def zipWithRowIndex: immutable.IndexedSeq[(DenseVector[A], Int)] = {
      (0 to denseMatrix.rows).map{
        rowIndex =>
          (denseMatrix(rowIndex, ::).inner, rowIndex)
      }
    }
  }

}


object BreezeImplicits extends BreezeImplicits
