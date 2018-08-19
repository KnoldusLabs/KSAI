package ksai.data.parser

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.collection.mutable

case class AttributeMeta(name: String = "", typ: String = "")


case class ARFF[A](
                    relation: String = "",
                    attributes: List[AttributeMeta] = Nil,
                    labels: List[A] = Nil,
                    isDataPresent: Boolean = false,
                    data: List[Array[Double]] = Nil,
                    target: List[A] = Nil) {

  private def labelMap: Map[A, Int] = {
    labels.zipWithIndex.map { case (l, index) => l -> index }.toMap
  }

  def getNumericTargets: List[Int] = {
    target.map(tgt => labelMap(tgt))
  }

  def getNumericsForRegressions = {
    target.map {
      case t: String => t.toDouble
      case _ => throw new IllegalArgumentException("IT cannot be anything apart from string")
    }
  }

}

case class Delimited[A](
                         labels: List[A] = Nil,
                         data: List[Array[Double]] = Nil,
                         target: List[A] = Nil) {

  def labelMap: Map[A, Int] = {
    labels.zipWithIndex.map { case (l, index) => l -> index }.toMap
  }

  def getNumericTargets: List[Int] = {
    target.map(tgt => labelMap(tgt))
  }

}

case class DelimitedRefactored[A](
                         labels: List[A] = Nil,
                         data: List[Array[Double]] = Nil,
                         target: List[A] = Nil,
                         labelMap: Map[A, Int] = Map.empty[A, Int]
                       ) {

  def getNumericTargets: List[Int] = {
    target.map(tgt => labelMap(tgt))
  }

}
