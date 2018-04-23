package ksai.core.classification

import ksai.data.parser.{ARFF, ARFFParser}
import ksai.math.Distribution
import ksai.training.validation.{LOOCV, ValidationImplicits}
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ArrayBuffer

class NaiveBayesTest extends WordSpec with Matchers with ValidationImplicits {



  val feature: Array[String] = Array(
    "outstanding", "wonderfully", "wasted", "lame", "awful", "poorly",
    "ridiculous", "waste", "worst", "bland", "unfunny", "stupid", "dull",
    "fantastic", "laughable", "mess", "pointless", "terrific", "memorable",
    "superb", "boring", "badly", "subtle", "terrible", "excellent",
    "perfectly", "masterpiece", "realistic", "flaws")

  "NaiveBayes" should {

    "be able to test of predict method" in {
      pending
      println(s"prediction starts ")

      val arffFile: String = getClass.getResource("/iris.arff").getPath
      val arff: ARFF[String] = ARFFParser.parse(arffFile)
      val n = arff.data.length
      val loocv: LOOCV = LOOCV(n)
      val bufferedData = arff.data.map(_.to[ArrayBuffer]).to[ArrayBuffer]
      val bufferedNumericTargets = arff.getNumericTargets.to[ArrayBuffer]
      var error = 0
      (0 until n).foreach{ index =>
        val trainx: ArrayBuffer[ArrayBuffer[Double]] = LOOCV.slice(bufferedData, loocv.train(index))
        val trainy: ArrayBuffer[Int] = LOOCV.sliceY(bufferedNumericTargets, loocv.train(index))
        val p = trainx(0).length
        val k = trainy.max + 1

        val priori: Array[Double] = Array[Double](k)
        val condprob:Array[Array[Distribution]] = Array(Array())
      }

    }


    "be able to test of learn method" in {
      pending
      println("batch learn Multinomial")

    }

  }

}
