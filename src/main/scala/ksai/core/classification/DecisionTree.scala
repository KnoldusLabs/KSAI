package ksai.core.classification

import ksai.core.classification.Node.{getAttributeWithMaxGain, isPure}
import ksai.data.parser.{Attribute, NominalAttribute}

import scala.collection.immutable

case class DecisionTree(attributeList: List[Attribute] = Nil,
                              label: Attribute,
                              trainingInstances: List[List[Double]],
                              responses: List[Int],
                              /*maxNodes: Int = 200,
                              nodeSize: Int = 1,
                              maxTry: Int = 15,
                              rule: SplitRule = ENTROPY,
                              samples: List[Int] = Nil,
                              order: List[List[Int]] = Nil*/) {
  //  private val classes = responses.distinct.sorted

  //  require(/*classes.sum != ((classes.size - 1)/2) * classes.size*/ classes.sum == 1 && classes.lengthCompare(2) == 0)

  private val ((attribute, index), remainingAttributes, instancesForEachValue) = getAttributeWithMaxGain(attributeList.zipWithIndex, trainingInstances.zip(responses))

  private val rootNode = LeafNode(attribute, index, -1).include(remainingAttributes, instancesForEachValue)

  println(rootNode)

  private val header =
    """digraph DecisionTree {
      |node [shape=box, style="filled, rounded", color="black", fontname=helvetica];
      |edge [fontname=helvetica];""".stripMargin

  private def createGraph(stringWithIndex: (String, Int), parentIndex: Int, label: String, node: Node, target: Attribute): (String, Int) = {
    val index = stringWithIndex._2

    (node, node.attribute, target) match {
      case (filledNode: FilledNode, nominalAttribute: NominalAttribute, _) =>
        val link = if (index != 0) s"""$parentIndex -> $index [labeldistance=2.5, labelangle=45, headlabel="$label"];\n""" else ""
        val node = s"""$index [label=<${filledNode.attribute.name}>, fillcolor="#00000000"];\n"""
        nominalAttribute.values.zipWithIndex.foldLeft(stringWithIndex) {
          case ((string, i), (value, attributeIndex)) =>
            createGraph((string + node + link, i + 1), index, value, filledNode.children(attributeIndex), target)
        }

      case (leafNode: LeafNode, _, targetAttribute: NominalAttribute) =>
        val node = s"""$index [label=<output = ${targetAttribute.values(leafNode.output)}>, fillcolor="#00000000", shape=ellipse];\n"""
        val link = if(index != 0) s"""$parentIndex -> $index [labeldistance=2.5, labelangle=45, headlabel="$label"];\n""" else ""
        (stringWithIndex._1 + node + link, index)
    }
  }

  def dot: String = header + createGraph(("", 0), -1, "", rootNode, label)._1 +"}"

 /* private def createGraph(stringWithIndex: (String, Int), parentIndex: Int, label: String, node: Node, result: String): (String, Int) = (node, node.attribute) match {
    case (filledNode: FilledNode, attribute: NominalAttribute) =>
      val index = stringWithIndex._2
      val link = if(index != 0) s"""$parentIndex -> $index [labeldistance=2.5, labelangle=45, headlabel="$label"];\n""" else ""

      val node = s"""$index [label=<${filledNode.attribute.name}>, fillcolor="#00000000"];\n"""
      attribute.values match {
        case falseLabel :: trueLabel :: Nil =>
          val falseChild = createGraph((stringWithIndex._1 + node + link, index + 1), index, falseLabel, filledNode.falseChild, "false")
          createGraph(falseChild, index, trueLabel, filledNode.trueChild, "true")
        case _ => throw new IllegalArgumentException(s"Invalid attribute: $attribute")
      }
    case (_: LeafNode, _) =>
      val index = stringWithIndex._2
      val node = s"""$index [label=<output = $result>, fillcolor="#00000000", shape=ellipse];\n"""
      val link = if(index != 0) s"""$parentIndex -> $index [labeldistance=2.5, labelangle=45, headlabel="$label"];\n""" else ""
      (stringWithIndex._1 + node + link, index + 1)
  }*/

  def predict(instance: List[Double]): Int = rootNode.predict(instance)
}


trait Node{

  type Data = List[List[Double]]

  def attribute: Attribute

  def include(remainingAttributes: List[(Attribute, Int)], distributedInstances: Seq[List[(List[Double], Int)]]): Node

  def predict(instance: List[Double]): Int

}

case class LeafNode(attribute: Attribute, attribIndex: Int, output: Int) extends Node{

  def include(remainingAttributes: List[(Attribute, Int)], distributedInstances: Seq[List[(List[Double], Int)]]): Node = {
    val emptyLeafNode = LeafNode(attribute, attribIndex, output)
    val children: Seq[Node] = distributedInstances.map{ instancesForValue: List[(List[Double], Int)] =>
      instancesForValue match {
        case Nil => emptyLeafNode
        case (_, response) :: _ if isPure(instancesForValue.map(_._2)) => LeafNode(attribute, attribIndex, response)
        case _ :: _ if instancesForValue.nonEmpty =>
          val ((childAttribute, childIndex), remainingCandidates, childDistributions) = getAttributeWithMaxGain(remainingAttributes, instancesForValue)
          LeafNode(childAttribute, childIndex, output).include(remainingCandidates, childDistributions)
        case _ if instancesForValue.nonEmpty => throw new IllegalArgumentException("Inconsistent data")
      }
    }

    FilledNode(attribute, attribIndex, children)
  }

  override def predict(instance: List[Double]): Int = output
}

case class FilledNode(attribute: Attribute, attributeIndex: Int, children: Seq[Node]) extends Node {

  def include(remainingAttributes: List[(Attribute, Int)], distributedInstances: Seq[List[(List[Double], Int)]]): Node =
    this.copy(children = children.map(node => node.include(remainingAttributes, distributedInstances)))

  override def predict(instance: List[Double]): Int = {
    children.zipWithIndex.find{case (_, label) => instance(attributeIndex) == label} match {
      case Some((node, _)) => node.predict(instance)
      case None => -1
    }
  }
}

object Node{

  def getAttributeWithMaxGain(attributesWithIndex: List[(Attribute, Int)], instancesWithResponse: List[(List[Double], Int)])
  : ((Attribute, Int), List[(Attribute, Int)], Seq[List[(List[Double], Int)]]) = {

    val responses = instancesWithResponse.map(_._2).distinct.sorted
    val instanceCountForEachLabel = responses.map(response => instancesWithResponse.count { case (_, r) => response == r }.toDouble)
    val entropy = getEntropy(instanceCountForEachLabel, instancesWithResponse.size)

    val ((attribute, index), _, instancesForEachValue) = attributesWithIndex.foldLeft(((NominalAttribute(""), -1), -1.0, Seq.empty[List[(List[Double], Int)]])) {
      case (((old, oldIndex), oldGain, oldList), (current: NominalAttribute, currIndex)) =>

        val instancesForEachValue: immutable.Seq[List[(List[Double], Int)]] = current.values.indices.map{ value =>
          instancesWithResponse.filter{
            case (instance, _) => instance(currIndex) == value
          }
        }

        val individualEntropies = instancesForEachValue.map { valueInstances =>
          val responseCount = responses.map(response => valueInstances.count { case (_, r) => response == r }.toDouble)
          getEntropy(responseCount, valueInstances.size)
        }

        val gain = entropy - individualEntropies.sum

        if(gain >= oldGain) ((current, currIndex), gain, instancesForEachValue)
        else ((old, oldIndex), oldGain, oldList)
    }

    val remainingAttributes = attributesWithIndex.filterNot(_._1 == attribute)

    ((attribute, index), remainingAttributes, instancesForEachValue)
  }

  def getEntropy(counts: List[Double], total: Double): Double = {
    if(counts.sum == total){
      if(counts.contains(total))
        0.0
      else
        - counts.map(count => (count/total) * (Math.log(count / total) / Math.log(2))).sum
    } else {
      throw new IllegalArgumentException("invalid data")
    }
  }

  def isPure(list: List[Int]): Boolean = list.distinct.lengthCompare(1) == 0

/*  FilledNode(
    NominalAttribute(windy,None,1.0,List(TRUE, FALSE)),
    3,
    Vector(
      FilledNode(
        NominalAttribute(outlook,None,1.0,List(sunny, overcast, rainy)),
        0,
        Vector(
          LeafNode(NominalAttribute(outlook,None,1.0,List(sunny, overcast, rainy)),0,0),
          LeafNode(NominalAttribute(outlook,None,1.0,List(sunny, overcast, rainy)),0,1),
          LeafNode(NominalAttribute(outlook,None,1.0,List(sunny, overcast, rainy)),0,0)
        )
      ),
      FilledNode(
        NominalAttribute(humidity,None,1.0,List(high, normal)),
        2,
        Vector(
          FilledNode(
            NominalAttribute(temperature,None,1.0,List(hot, cool)),
            1,
            Vector(
              FilledNode(
                NominalAttribute(outlook,None,1.0,List(sunny, overcast, rainy)),
                0,
                Vector(
                  LeafNode(NominalAttribute(outlook,None,1.0,List(sunny, overcast, rainy)),0,0),
                  LeafNode(NominalAttribute(outlook,None,1.0,List(sunny, overcast, rainy)),0,1),
                  LeafNode(NominalAttribute(outlook,None,1.0,List(sunny, overcast, rainy)),0,-1)
                )
              ),
              LeafNode(NominalAttribute(temperature,None,1.0,List(hot, cool)),1,1)
            )
          ),
          LeafNode(NominalAttribute(humidity,None,1.0,List(high, normal)),2,1)
        )
      )
    )
  )*/

}
