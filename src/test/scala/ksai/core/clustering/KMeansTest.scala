package ksai.core.clustering

import breeze.linalg.DenseMatrix
import ksai.core.cluster.KMeans
import ksai.data.parser.{ARFF, ARFFParser, Delimited, DelimitedParser}
import ksai.training.validation.ValidationImplicits
import ksai.validation.RandIndex
import org.scalatest.{FlatSpec, Matchers}


class KMeansTest extends FlatSpec with Matchers with ValidationImplicits {


  "K-Means" should "be able to apply separate files validation with LMS USPS" in {
    val zipTraingPath = getClass.getResource("/zip.train").getPath
    val zipTestPath = getClass.getResource("/zip.test").getPath
    val delimited: Delimited[String] = DelimitedParser.parse(zipTraingPath)
    val delimitedTest: Delimited[String] = DelimitedParser.parse(zipTestPath)
    val inputNodesNum = delimited.data.head.size

    val kmeans: KMeans = KMeans(delimited.data.map(_.toList), 10, 100, 4)

    val r1 = RandIndex.measure(delimited.getNumericTargets, kmeans.y)

    assert(r1 > 0.85)
  }


}
