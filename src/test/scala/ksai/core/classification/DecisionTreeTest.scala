package ksai.core.classification

import ksai.data.parser.{ARFFParser, AttributeMetaData}
import org.scalatest.{FlatSpec, Matchers}

class DecisionTreeTest extends FlatSpec with Matchers{

  val arffFile = getClass.getResource("/weatherNew.nominal.arff").getPath
  val weather: AttributeMetaData = ARFFParser.parseNominal(arffFile)
  val arffTestFile = getClass.getResource("/weatherNewRF.nominal.arff").getPath
  val weatherTest = ARFFParser.parseNominal(arffTestFile)


  it should "match same file" in{
    val trainingInstances = weather.data.map(_.toList)
    val dTree = DecisionTree(weather.attributes, weather.label, trainingInstances, weather.target/*, 200, 1, 10, GINI, Nil, Nil*/)
    val responses = trainingInstances.indices.map{ index =>
      val output = dTree.predict(trainingInstances(index))
      output == weather.target(index)
    }
    assert(responses.count(_ == false) == 0)
  }

  it should "match test file" in{
    val trainingInstances = weather.data.map(_.toList)
    val testInstances = weatherTest.data.map(_.toList)
    val dTree = DecisionTree(weather.attributes, weather.label, trainingInstances, weather.target/*, 200, 1, 10, GINI, Nil, Nil*/)
    val responses = testInstances.indices.map{ index =>
      val output = dTree.predict(testInstances(index))
      println(s"for ${testInstances(index)} out put is $output instead of ${weatherTest.target(index)}")
      output == weatherTest.target(index)
    }
    assert(responses.count(_ == false) == 0)
  }

  /*it should "print the structure of the tree" in {
    val trainingInstances = weather.data.map(_.toList)
    val dTree = DecisionTree(weather.attributes, weather.label, trainingInstances, weather.target/*, 200, 1, 10, GINI, Nil, Nil*/)
    val dotGraph = dTree.dot
    println(s"Paste the following in the url : http://viz-js.com/\n\n$dotGraph\n\n")
    assert(dotGraph.nonEmpty)
  }*/
}