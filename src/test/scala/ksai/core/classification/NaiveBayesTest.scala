package ksai.core.classification

import ksai.core.classification.NaiveBayesTest.x
import ksai.data.parser.{ARFF, ARFFParser}
import ksai.math.Distribution
import ksai.training.validation.{LOOCV, ValidationImplicits}
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class NaiveBayesTest extends WordSpec with Matchers with ValidationImplicits {

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
      val loocv = LOOCV(x.length)
      var error = 0
      var total = 0
      LOOCV.slice(loocv.train, ArrayBuffer)
    }

  }

}

object NaiveBayesTest{

  val feature: Array[String] = Array(
    "outstanding", "wonderfully", "wasted", "lame", "awful", "poorly",
    "ridiculous", "waste", "worst", "bland", "unfunny", "stupid", "dull",
    "fantastic", "laughable", "mess", "pointless", "terrific", "memorable",
    "superb", "boring", "badly", "subtle", "terrible", "excellent",
    "perfectly", "masterpiece", "realistic", "flaws")

  val movieX = new Array[Array[Double]](2000)
  val movieY = new Array[Int](2000)

  val x = new Array[Array[String]](2000)

  val resource = Source.fromFile("src/test/resources/movie.txt").getLines().toArray

  (0 until 2000).foreach { itr =>
    val value = resource(itr)
    val words = value.trim.split(" ")
    println("*****" + words(0))
    if(words(0).equalsIgnoreCase("pos")){
      movieY(itr) = 1
    } else if(words(0).equalsIgnoreCase("neg")) {
      movieY(itr) = 0
    } else println("Invalid class label: " + words(itr))
    x(itr) = words
  }

  x.indices.foreach{itr =>
    movieX(itr) = feature(x(itr))
  }

  //  (0 until 2000).foreach{ iterator =>}
  val (featureMap, _) = feature.foldLeft((Map.empty[String, Int], 0)){
    case ((map, k), string) if !map.keySet.contains(string) => (map ++ Map(string -> (k + 1)), k + 1)
    case (tuple, _) => tuple
  }

  def feature(x: Array[String]): Array[Double] = {
    val bag = new Array[Double](featureMap.size)
    x.foreach{word =>
      featureMap.get(word).foreach{f=> bag(f) = bag(f) + 1}
    }
    bag
  }

}