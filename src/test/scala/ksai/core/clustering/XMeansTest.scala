package ksai.core.clustering

import ksai.core.cluster.XMeans
import ksai.data.parser.{Delimited, DelimitedParser}
import ksai.multithreading.KMeansFactory
import ksai.training.validation.ValidationImplicits
import ksai.validation.RandIndex
import org.scalatest.{AsyncFlatSpec, Matchers}


class XMeansTest extends AsyncFlatSpec with Matchers with ValidationImplicits {

  "X-Means" should "be able to apply separate files validation with LMS USPS" in {
    val zipTraingPath = getClass.getResource("/zip.train").getPath
    val zipTestPath = getClass.getResource("/zip.test").getPath
    val delimited: Delimited[String] = DelimitedParser.parse(zipTraingPath)
    val delimitedTest: Delimited[String] = DelimitedParser.parse(zipTestPath)
    val inputNodesNum = delimited.data.head.size

    XMeans(delimited.data.map(_.toList), 10).map {
      case xmeans =>
        val r1 = RandIndex.measure(delimited.getNumericTargets, xmeans.y)
        println(s"XMeans number of clusters is ${xmeans.k}")
        KMeansFactory.system.terminate()
        assert(r1 > 0.85)
    }

  }

}
