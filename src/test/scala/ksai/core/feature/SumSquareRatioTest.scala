package ksai.core.feature

import ksai.data.parser.ARFFParser
import ksai.training.validation.ValidationImplicits
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer


class SumSquareRatioTest extends FlatSpec with Matchers with ValidationImplicits {

  "SumSquareRatio" should "rank the feature right way" in {
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

    val ratio = SumSquareRatio.rank(arff.data.map(_.to[ArrayBuffer]).to[ArrayBuffer], y)
    assert(4 == ratio.length)
    assert(0.6865388879809751 == ratio(0))
    assert(ratio(0) > 1E-7)
  }

}
