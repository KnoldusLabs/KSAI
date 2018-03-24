package ksai.core.classification

import ksai.data.parser.ARFFParser
import org.scalatest.{FlatSpec, Matchers}

class BinaryDecisionTreeTest extends FlatSpec with Matchers{

  val arffFile = getClass.getResource("/weather.nominal.arff").getPath
  val weather = ARFFParser.parseNominal(arffFile)
  val arffTestFile = getClass.getResource("/weatherRF.nominal.arff").getPath
  val weatherTest = ARFFParser.parseNominal(arffTestFile)


  it should "match same file" in{
    val trainingInstances = weather.data.map(_.toList)
    val dTree = BinaryDecisionTree(weather.attributes, weather.label, trainingInstances, weather.target/*, 200, 1, 10, GINI, Nil, Nil*/)
    val responses = trainingInstances.indices.map{ index =>
      val output = dTree.predict(trainingInstances(index))
      output == weather.target(index)
    }
    assert(responses.count(_ == false) == 0)
  }

  it should "match test file" in{
    val trainingInstances = weather.data.map(_.toList)
    val testInstances = weatherTest.data.map(_.toList)
    val dTree = BinaryDecisionTree(weather.attributes, weather.label, trainingInstances, weather.target/*, 200, 1, 10, GINI, Nil, Nil*/)
    val responses = testInstances.indices.map{ index =>
      val output = dTree.predict(testInstances(index))
      output == weatherTest.target(index)
    }
    assert(responses.count(_ == false) == 0)
  }

  it should "print the structure of the tree" in {
    val trainingInstances = weather.data.map(_.toList)
    val dTree = BinaryDecisionTree(weather.attributes, weather.label, trainingInstances, weather.target/*, 200, 1, 10, GINI, Nil, Nil*/)
    val dotGraph = dTree.dot
    println(s"Paste the following in the url : http://viz-js.com/\n\n$dotGraph\n\n")
    assert(dotGraph.nonEmpty)
  }
}
