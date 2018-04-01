package ksai.core.feature

import ksai.data.parser.{ARFFParser, Delimited, DelimitedParser}
import ksai.training.validation.{LOOCV, ValidationImplicits}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer


class SingleNoiseRatioTest extends FlatSpec with Matchers with ValidationImplicits {

  "SingleNoiseRatio" should "rank the feature right way" in {
    val arffFile = getClass.getResource("/iris.arff").getPath
    val arff = ARFFParser.parse(arffFile)
    val y = arff.getNumericTargets.to[ArrayBuffer]
    (0 to y.length - 1).foreach { index =>
      if (y(index) < 2) {
        y(index) = 0
      } else {
        y(index) = 1
      }
    }

    val ratio = SingleNoiseRatio.rank(arff.data.map(_.to[ArrayBuffer]).to[ArrayBuffer], y)
    assert(4 == ratio.length)
    assert(0.874310650941469 == ratio(0))
    assert(ratio(0) > 1E-7)
  }

}
