package ksai.core.classification

import ksai.core.classification.Node.{getAttributeWithMaxGain, isPure}
import ksai.data.parser.{Attribute, NominalAttribute}

case class BinaryDecisionTree(attributeList: List[Attribute] = Nil,
                              label: Attribute = NominalAttribute(""),
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

//  private val attribute = getAttributeWithMaxGain(attributeList.zipWithIndex, trainingInstances.zip(responses))
  private val (attribute, remainingAttributes, falseInstances, trueInstances) =
    getAttributeWithMaxGain(attributeList.zipWithIndex, trainingInstances.zip(responses))

  private val rootNode = {
    LeafNode(attribute._1, attribute._2, -1).include(remainingAttributes, falseInstances, trueInstances)
  }

  private val header =
    """digraph DecisionTree {
      |node [shape=box, style="filled, rounded", color="black", fontname=helvetica];
      |edge [fontname=helvetica];""".stripMargin

  def dot: String = header + createGraph(("", 0), -1, "", rootNode, "")._1 +"}"

  private def createGraph(stringWithIndex: (String, Int),parentIndex: Int, label: String, node: Node, result: String): (String, Int) = (node, node.attribute) match {
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
  }

  def predict(instance: List[Double]): Int = rootNode.predict(instance)
}


trait Node{

  type Data = List[List[Double]]

  def attribute: Attribute

  def include(remainingAttributes: List[(Attribute, Int)], trueInstances: List[(List[Double], Int)],
              falseInstances: List[(List[Double], Int)]): Node

  def predict(instance: List[Double]): Int

}

case class LeafNode(attribute: Attribute, attribIndex: Int, output: Int) extends Node{

  def include(remainingAttributes: List[(Attribute, Int)], falseInstancesWithResponse: List[(List[Double], Int)],
              trueInstancesWithResponse: List[(List[Double], Int)]): Node = {
    val emptyLeafNode = LeafNode(attribute, attribIndex, output)
    val isFalsePure = isPure(falseInstancesWithResponse.map(_._2))
    val isTruePure = isPure(trueInstancesWithResponse.map(_._2))

    (trueInstancesWithResponse, falseInstancesWithResponse) match {
      case (Nil, Nil) => this
      case (Nil, (_, falseResponse) :: _) if remainingAttributes.isEmpty && isFalsePure=>
        val falseLeafNode = LeafNode(attribute, attribIndex, falseResponse)
        FilledNode(attribute, attribIndex, falseLeafNode, emptyLeafNode)
      case (Nil, (_, falseResponse) :: _) if remainingAttributes.nonEmpty=>
        val falseLeafNode = if(isFalsePure){
          LeafNode(attribute, attribIndex, falseResponse)
        } else {
          val falseChild = getAttributeWithMaxGain(remainingAttributes, falseInstancesWithResponse)
          LeafNode(falseChild._1._1, falseChild._1._2, output).include(falseChild._2, falseChild._3, falseChild._4)
        }

        FilledNode(attribute, attribIndex, falseLeafNode, emptyLeafNode)
      case ((_, trueResponse) :: _, Nil) if remainingAttributes.isEmpty && isTruePure=>
        val trueLeafNode = LeafNode(attribute, attribIndex, trueResponse)
        FilledNode(attribute, attribIndex, emptyLeafNode, trueLeafNode)
      case ((_, trueResponse) :: _, Nil) if remainingAttributes.nonEmpty=>
        val trueLeafNode = if(isTruePure){
            LeafNode(attribute, attribIndex, trueResponse)
        } else {
          val trueChild = getAttributeWithMaxGain(remainingAttributes, trueInstancesWithResponse)
          LeafNode(trueChild._1._1, trueChild._1._2, output).include(trueChild._2, trueChild._3, trueChild._4)
        }

        FilledNode(attribute, attribIndex, emptyLeafNode, trueLeafNode)

      case ((_, trueResponse) :: _, (_, falseResponse) :: _) if remainingAttributes.isEmpty && isFalsePure && isTruePure=>
        val trueLeafNode = LeafNode(attribute, attribIndex, trueResponse)
        val falseLeafNode = LeafNode(attribute, attribIndex, falseResponse)
        FilledNode(attribute, attribIndex, falseLeafNode, trueLeafNode)
      case ((_, trueResponse) :: _, (_, falseResponse) :: _) if remainingAttributes.nonEmpty=> //TODO:add support for multiple children
        val trueLeafNode = LeafNode(attribute, attribIndex, trueResponse)
        val falseLeafNode = LeafNode(attribute, attribIndex, falseResponse)

        val (fChild, tChild) = if (isFalsePure && isTruePure) {
          (falseLeafNode, trueLeafNode)
        } else if (isFalsePure) {
          val trueChild = getAttributeWithMaxGain(remainingAttributes, trueInstancesWithResponse)
          (falseLeafNode, LeafNode(trueChild._1._1, trueChild._1._2, output).include(trueChild._2, trueChild._3, trueChild._4))
        } else if (isTruePure) {
          val falseChild = getAttributeWithMaxGain(remainingAttributes, falseInstancesWithResponse)
          (LeafNode(falseChild._1._1, falseChild._1._2, output).include(falseChild._2, falseChild._3, falseChild._4), trueLeafNode)
        } else {
          val trueChild = getAttributeWithMaxGain(remainingAttributes, trueInstancesWithResponse)
          val falseChild = getAttributeWithMaxGain(remainingAttributes, falseInstancesWithResponse)
          (
            LeafNode(falseChild._1._1, falseChild._1._2, output).include(falseChild._2, falseChild._3, falseChild._4),
            LeafNode(trueChild._1._1, trueChild._1._2, output).include(trueChild._2, trueChild._3, trueChild._4)
          )
        }

        FilledNode(attribute, attribIndex, fChild, tChild)
      case _ if remainingAttributes.isEmpty => throw new IllegalArgumentException("Inconsistent data")
    }
  }

  override def predict(instance: List[Double]): Int = {
    output
  }
}

case class FilledNode(attribute: Attribute,  attribIndex: Int, falseChild: Node, trueChild: Node) extends Node{
  override def include(remainingAttributes: List[(Attribute, Int)], trueInstances: List[(List[Double], Int)],
    falseInstances: List[(List[Double], Int)]): Node = {
    this.copy(
      trueChild = trueChild.include(remainingAttributes, trueInstances, falseInstances),
      falseChild = falseChild.include(remainingAttributes, trueInstances, falseInstances)
    )
  }

  override def predict(instance: List[Double]): Int = {//TODO:refactor this method
    if(instance(attribIndex).toInt != 0) trueChild.predict(instance)
    else falseChild.predict(instance)
  }
}

object Node{

  def getAttributeWithMaxGain(attributesWithIndex: List[(Attribute, Int)], instancesWithResponse: List[(List[Double], Int)])
  : ((Attribute, Int), List[(Attribute, Int)], List[(List[Double], Int)], List[(List[Double], Int)]) = {

    val negativeInstancesWithResponse: List[(List[Double], Int)] = instancesWithResponse.filter{ case(_, response) => response == 0}
    val positiveInstancesWithResponse: List[(List[Double], Int)] = instancesWithResponse.filter{ case(_, response) => response == 1}
    val entropy = getEntropy(negativeInstancesWithResponse.size, positiveInstancesWithResponse.size, instancesWithResponse.size)

    val ((attribute, index), _) = attributesWithIndex.foldLeft(((NominalAttribute(""), -1), -1.0)) {
      case (((old, oldIndex), oldGain), (current: NominalAttribute, currIndex)) =>
        //TODO:add support for multiple children
        val trueInstances = instancesWithResponse.filter{
          case(instance, _) => instance(currIndex) == 1
        }

        val falseInstances = instancesWithResponse.filter{
          case(instance, _) => instance(currIndex) == 0
        }

        if (trueInstances.isEmpty || falseInstances.isEmpty) {
          ((old, oldIndex), oldGain)
        } else {
          val tfc = trueInstances.count(_._2 == 0)
          val ttc = trueInstances.count(_._2 != 0)
          val ffc = falseInstances.count(_._2 == 0)
          val ftc = falseInstances.count(_._2 != 0)
          val trueEntropy = getEntropy(tfc, ttc, trueInstances.size)
          val falseEntropy = getEntropy(ffc, ftc, falseInstances.size)
          val trueRatio = trueInstances.size.toDouble / instancesWithResponse.size
          val falseRatio = falseInstances.size.toDouble / instancesWithResponse.size
          val addOn = (trueRatio * trueEntropy) + (falseRatio * falseEntropy)
          val gain = entropy - addOn
          if (gain >= oldGain || oldIndex == -1) ((current, currIndex), gain) else ((old, oldIndex), oldGain)
        }
    }

    val trueInstances = instancesWithResponse.filter(list => list._1(index) == 1)
    val falseInstances = instancesWithResponse.filter(list => list._1(index) == 0)
    val remainingAttributes = attributesWithIndex.filterNot(_._1 == attribute)
    ((attribute, index), remainingAttributes, falseInstances, trueInstances)
  }

  def getEntropy(falseCount: Double, trueCount: Double, count: Double): Double = {
    if(trueCount + falseCount == count){
      if(falseCount == 0 || trueCount == 0)
        0.0
      else
        -((trueCount / count) * (Math.log(trueCount / count) / Math.log(2)) + (falseCount / count) * (Math.log(falseCount / count) / Math.log(2)))
    }
    else throw new IllegalArgumentException("invalid data")
  }

  def isPure(list: List[Int]): Boolean = list.distinct.lengthCompare(1) == 0

}
