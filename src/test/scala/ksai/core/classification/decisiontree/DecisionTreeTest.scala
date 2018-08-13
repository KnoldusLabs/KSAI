package ksai.core.classification.decisiontree

import akka.actor.ActorSystem
import akka.util.Timeout
import ksai.data.parser.{ARFFParser, Delimited, DelimitedParser}
import ksai.training.validation.LOOCV
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.io.Source

class DecisionTreeTest extends WordSpec with Matchers {

  implicit val actorSystem = ActorSystem("Decision-Tree")
  implicit val timeout = Timeout(10 seconds)

  "Decision Tree" should {

    "should test iris file" in {
      val irisFile = getClass.getResource("/iris.arff").getPath
      val iris = ARFFParser.parse(irisFile)
      val x = iris.data.toArray.map(_.to[ArrayBuffer]).to[ArrayBuffer]
      val y = iris.getNumericTargets.to[ArrayBuffer]

      val n = x.length

      val loocv = LOOCV(n)

      val error = (0 until n).foldLeft(0) { (err, i) =>
        val trainX = LOOCV.slice(x, loocv.train(i)).toArray.map(_.toArray)
        val trainY: Array[Int] = LOOCV.sliceY(y, loocv.train(i)).toArray
        val decisionTree = DecisionTree(trainX, trainY, 4)
        if (y(loocv.test(i)) != decisionTree.predict(x(loocv.test(i)).toArray)) {
          err + 1
        } else {
          err
        }
      }

      println("Decision Tree error = " + error)
      assert(error == 5)
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

      val dTree = DecisionTree(None, x, x1, 350, SplitRule.ENTROPY)

      val error = y.indices.map{ index =>
        if (dTree.predict(y(index)) != y1(index)) 1 else 0
      }.sum

      println("Decision Tree error = " + error)
      assert(error == 323)
    }
  }
}
