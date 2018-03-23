package ksai.data.parser

import breeze.linalg.{DenseMatrix, DenseVector}

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
    target.map{
      case t: String => t.toDouble
      case _ => throw new IllegalArgumentException("IT cannot be anything apart from string")
    }
  }

}

case class Delimited[A](
                         labels: List[A] = Nil,
                         data: List[Array[Double]] = Nil,
                         target: List[A] = Nil) {

  private def labelMap: Map[A, Int] = {
    labels.zipWithIndex.map { case (l, index) => l -> index }.toMap
  }

  def getNumericTargets: List[Int] = {
    target.map(tgt => labelMap(tgt))
  }

}


trait Attribute {
  def name: String

  def description: Option[String]

  def weight: Double
}

case class NumericAttribute(name: String, description: Option[String], weight: Double = 1.0) extends Attribute


case class NominalAttribute(name: String, description: Option[String] = None, weight: Double = 1.0, values: List[String] = Nil) extends Attribute {
  def size: Int = values.size
}

case class AttributeMetaData(relation: String = "",
                             attributes: List[Attribute] = Nil,
                             label: Attribute = NominalAttribute(""),
                             isDataPresent: Boolean = false,
                             data: List[Array[Double]] = Nil,
                             target: List[Int] = Nil)