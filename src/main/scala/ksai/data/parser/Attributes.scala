package ksai.data.parser

import breeze.linalg.{DenseMatrix, DenseVector}

case class AttributeMeta(name: String = "", typ: String = "")

case class ARFF(
                 relation: String = "",
                 attributes: List[AttributeMeta] = Nil,
                 nominals: List[String] = Nil,
                 isDataPresent: Boolean = false,
                 data: List[Array[Double]] = Nil,
                 target: List[String] = Nil)