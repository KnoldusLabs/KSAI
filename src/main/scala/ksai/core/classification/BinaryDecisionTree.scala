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

  def predict(instance: List[Double]) = rootNode.predict(instance)
}


trait Node{

  type Data = List[List[Double]]

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
      case _ if remainingAttributes.isEmpty => this
      case (Nil, Nil) => this
      case (Nil, (_, falseResponse) :: _) =>
        val falseChild = getAttributeWithMaxGain(remainingAttributes, falseInstancesWithResponse)
        val falseLeafNode = if(isFalsePure){
          LeafNode(attribute, attribIndex, falseResponse)
        } else {
          LeafNode(falseChild._1._1, falseChild._1._2, output).include(falseChild._2, falseChild._3, falseChild._4)
        }

        FilledNode(attribute, attribIndex, falseLeafNode, emptyLeafNode)
      case ((_, trueResponse) :: _, Nil) =>
        val trueChild = getAttributeWithMaxGain(remainingAttributes, trueInstancesWithResponse)
        val trueLeafNode = if(isTruePure){
            LeafNode(attribute, attribIndex, trueResponse)
        } else {
          LeafNode(trueChild._1._1, trueChild._1._2, output).include(trueChild._2, trueChild._3, trueChild._4)
        }

        FilledNode(attribute, attribIndex, emptyLeafNode, trueLeafNode)

      case ((_, trueResponse) :: _, (_, falseResponse) :: _) => //TODO:add support for multiple children
        val trueChild = getAttributeWithMaxGain(remainingAttributes, trueInstancesWithResponse)
        val falseChild = getAttributeWithMaxGain(remainingAttributes, falseInstancesWithResponse)
        val trueLeafNode = LeafNode(attribute, attribIndex, trueResponse)
        val falseLeafNode = LeafNode(attribute, attribIndex, falseResponse)

        val (fChild, tChild) = if (isFalsePure && isTruePure) {
          (falseLeafNode, trueLeafNode)
        } else if (isFalsePure) {
          (falseLeafNode, LeafNode(trueChild._1._1, trueChild._1._2, output).include(trueChild._2, trueChild._3, trueChild._4))
        } else if (isTruePure) {
          (LeafNode(falseChild._1._1, falseChild._1._2, output).include(falseChild._2, falseChild._3, falseChild._4), trueLeafNode)
        } else {
          (
            LeafNode(falseChild._1._1, falseChild._1._2, output).include(falseChild._2, falseChild._3, falseChild._4),
            LeafNode(trueChild._1._1, trueChild._1._2, output).include(trueChild._2, trueChild._3, trueChild._4)
          )
        }

        FilledNode(attribute, attribIndex, fChild, tChild)
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

    val ((attribute, index), _) = attributesWithIndex.tail.foldLeft((attributesWithIndex.head, 0.0)) {
      case (((old, oldIndex), oldGain), (current: NominalAttribute, currIndex)) =>
        val negativeInstancesWithResponse: List[(List[Double], Int)] = instancesWithResponse.filter{
          case(_, response) => response == 0
        }

        val positiveInstancesWithResponse: List[(List[Double], Int)] = instancesWithResponse.filter{
          case(_, response) => response == 1
        }

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
          val addOn = (trueInstances.size / instancesWithResponse.size) * entropy(trueInstances.count(_._2 == 0), trueInstances.count(_._2 != 0), trueInstances.size) +
            (falseInstances.size / instancesWithResponse.size) * entropy(falseInstances.count(_._2 == 0), falseInstances.count(_._2 != 0), falseInstances.size)
          val gain = entropy(negativeInstancesWithResponse.size, positiveInstancesWithResponse.size, instancesWithResponse.size) - addOn
          if (gain > oldGain) ((current, currIndex), gain) else ((old, oldIndex), oldGain)
        }
    }

    val trueInstances = instancesWithResponse.filter(list => list._1(index) == 1)
    val falseInstances = instancesWithResponse.filter(list => list._1(index) == 0)
    val remainingAttributes = attributesWithIndex.filterNot(_._1 == attribute)
    ((attribute, index), remainingAttributes, falseInstances, trueInstances)
  }

  def entropy(trueCount: Int, falseCount: Int, count: Int): Double = - ((trueCount/count) * (Math.log(trueCount/count)/Math.log(2)) +
    (falseCount/count) * (Math.log(falseCount/count)/Math.log(2)))

  def isPure(list: List[Int]): Boolean = list.distinct.lengthCompare(1) == 0

}
