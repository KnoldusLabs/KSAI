package ksai.core.classification

import ksai.training.validation.{CrossValidation, ValidationImplicits}
import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class NaiveBayesTest extends WordSpec with Matchers with ValidationImplicits {

  import ksai.core.classification.NaiveBayesTest._

  "NaiveBayes" should {

    "be able to test of learn method" in {
      println("batch learn Multinomial")
      val crossValidation = CrossValidation(movieX.length, 10)
      var error = 0
      var total = 0

      (0 until 10).foreach{ itr =>
        val trainX = sliceX(movieX, crossValidation.train(itr))
        val trainY = sliceY(movieY, crossValidation.train(itr))

        val naiveBayes = NaiveBayes(model = MULTINOMIAL, classCount = 2, independentVariablesCount = feature.size)
        naiveBayes.learn(trainX, trainY)

        val testX = sliceX(movieX, crossValidation.test(itr))
        val testY = sliceY(movieY, crossValidation.test(itr))

        testX.indices.foreach{ j =>
          val label = naiveBayes.predict(testX(j))
          if(label != -1){
            total = total + 1
            if(testY(j) != label){
              error = error + 1
            }
          }
        }
      }

      println(s"Multinomial error is $error of total $total")

      assert(error < 265)

    }

    "be able to test of learn method, of class SequenceNaiveBayes" in {
      println("batch learn Bernoulli")
      val crossValidation = CrossValidation(movieX.length, 10)
      var error = 0
      var total = 0

      (0 until 10).foreach { itr =>
        val trainX = sliceX(movieX, crossValidation.train(itr))
        val trainY = sliceY(movieY, crossValidation.train(itr))

        val naiveBayes = NaiveBayes(model = BERNOULLI, classCount = 2, independentVariablesCount = feature.size)
        naiveBayes.learn(trainX, trainY)

        val testX = sliceX(movieX, crossValidation.test(itr))
        val testY = sliceY(movieY, crossValidation.test(itr))

        testX.indices.foreach { j =>
          val label = naiveBayes.predict(testX(j))
          if(label != -1){
            total = total + 1
            if(testY(j) != label){
              error = error + 1
            }
          }
        }
      }
      println(s"Bernoulli error is $error of total $total")
      assert(error < 270)
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
    if(words(0).equalsIgnoreCase("pos")){
      movieY(itr) = 1
    } else if(words(0).equalsIgnoreCase("neg")) {
      movieY(itr) = 0
    } else println("Invalid class label: " + words(itr))
    x(itr) = words
  }

  val (featureMap, _) = feature.foldLeft((Map.empty[String, Int], 0)){
    case ((map, k), string) if !map.keySet.contains(string) => (map ++ Map(string -> k), k + 1)
    case (tuple, _) => tuple
  }

  x.indices.foreach{itr =>
    movieX(itr) = feature(x(itr))
  }


  def feature(x: Array[String]): Array[Double] = {
    val bag = new Array[Double](feature.size)
    x.foreach{word =>
      featureMap.get(word).foreach{f=> bag(f) = bag(f) + 1}
    }
    bag
  }

  def sliceX(data:Array[Array[Double]], index: Array[Int]): Array[Array[Double]] = {
    index.indices.map{ itr =>
      data(index(itr))
    }.toArray
  }

  def sliceY(data:Array[Int], index: Array[Int]): Array[Int] = {
    index.indices.map{ itr =>
      data(index(itr))
    }.toArray
  }
}