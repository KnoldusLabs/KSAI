/*
package ksai.core.classification

case class DecisionTree(attributeList: List[NumericAttribute] = Nil, trainingInstances: List[List[Double]], responses: List[Int], maxNodes: Int, nodeSize: Int, mtry: Int, rule: SplitRule, samples: List[Int] = Nil, orderList: List[List[Int]] = Nil){

  require(trainingInstances.lengthCompare(responses.length) == 0, String.format("The sizes of X and Y don't match: %d != %d", trainingInstances.length, responses.length))
  require(mtry < 1 || mtry > trainingInstances.head.length, "Invalid number of variables to split on at a node of the tree: " + mtry)
  require(maxNodes < 2, "Invalid maximum leaves: " + maxNodes)
  require(nodeSize < 1, "Invalid minimum size of leaf nodes: " + nodeSize)

  val labels: List[Int] = responses.foldLeft(List.empty[Int]) {
    case (labelList, label) if labelList.contains(label) => labelList
    case (labelList, label) => label :: labelList
  }.sorted

  val attributeCount: Int = trainingInstances match {
    case firstInstance :: _ => firstInstance.size
    case _ => 0
  }

  require(!labels.exists(_ < 0), "Negative class label: " + labels.find(_ < 0))

  require(labels.lengthCompare(labelCount) == 0, "Missing class")
  require(labelCount >= 2, "Only one class.")

  def labelCount: Int = labels match {
    case first :: _ => labels.reverse match{
      case last :: _ => last - first
      case _ => 0
    }
    case _ => 0
  }

  def attributes: List[NumericAttribute] ={
    attributeList match {
      case attributes if attributes.lengthCompare(attributeCount) == 0 => attributes
      case _ => (0 until attributeCount).map(index => NumericAttribute(name = "V" + (index + 1).toString, description = None)).toList
    }
  }

  def order: List[List[Int]] = orderList match {
    case Nil => trainingInstances.map(instance => instance.zipWithIndex.sortWith(_._1 < _._1).map(_._2))
    case order if order.lengthCompare(attributeCount) == 0 => order
  }

  val (count, samples) = samples match {
    case Nil =>
      val count = (0 until attributeCount).map (index => responses.count (_== index) ).toList
      val samples = (0 until attributeCount).map(_ => 1)
      (count, samples)
  }



  case class Node(output: Int = -1, posteriori: List[Double] = Nil)
}

case class NumericAttribute(name: String, description: Option[String], weight: Double = 1.0)

abstract class SplitRule
object GINI extends SplitRule
object ENTROPY extends SplitRule
object CLASSIFICATION_ERROR extends SplitRule*/
