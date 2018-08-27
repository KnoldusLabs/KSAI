package ksai.core.classification.randomforest

import akka.actor.ActorSystem
import akka.util.Timeout
import ksai.data.parser.{ARFFParser, DelimitedParser}
import ksai.training.validation.LOOCV
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

class RandomForestTest extends WordSpec with Matchers {

  implicit val actorSystem = ActorSystem("Random-Forest")
  implicit val timeout = Timeout(50 seconds)

  "Random Forest" should {
    "iris file" in {
      val weatherFile = getClass.getResource("/iris.arff").getPath
      val weather = ARFFParser.parse(weatherFile)
      val x = weather.data.toArray.map(_.to[ArrayBuffer]).to[ArrayBuffer]
      val y = weather.getNumericTargets.to[ArrayBuffer]

      val n = x.length

      val loocv = LOOCV(n)

      val error = (0 until n).foldLeft(0) { (err, i) =>
        val trainX = LOOCV.slice(x, loocv.train(i)).toArray.map(_.toArray)
        val trainY: Array[Int] = LOOCV.sliceY(y, loocv.train(i)).toArray
        val randomForest = RandomForest(trainX, trainY, 100)
        if (y(loocv.test(i)) != randomForest.predict(x(loocv.test(i)).toArray)) {
          err + 1
        } else {
          err
        }
      }

      println("Random Forest error = " + error)
      assert(error <= 9)
    }

    "zip files" in {
      val zipTrainFile = getClass.getResource("/zip.train").getPath
      val zipTestFile = getClass.getResource("/zip.test").getPath
      val trainFile = DelimitedParser.parseZip(zipTrainFile)
      val testFile = DelimitedParser.parseZip(zipTestFile)
      val x = trainFile.data.toArray.map(_.toArray)
      val x1 = trainFile.getNumericTargets.toArray
      val lblMap = trainFile.labelMap
      val y = testFile.data.toArray.map(_.toArray)
      val y1 = testFile.target.map(x => lblMap(x))

      val randomForest = RandomForest(x, x1, 200)

      val error = y.indices.map { index =>
        if (randomForest.predict(y(index)) != y1(index)) 1 else 0
      }.sum

      println("Random Forest error = " + error)
      assert(error <= 225)
    }
  }
}
