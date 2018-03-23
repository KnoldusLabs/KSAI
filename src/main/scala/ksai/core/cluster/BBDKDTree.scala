package ksai.core.cluster

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import ksai.multithreading.KAsyncExec._
import ksai.multithreading._

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration._

case class BBDKDTree(
                      root: BBDKDNode,
                      index: List[Int]
                    ) {

  /**
    * Lowerbound is basically the collection of minimum of each feature.
    * Upperbound is basically the collection of maximum of each feature.
    *
    * @return
    */
  private def getLowerAndUpperBound(data: ListBuffer[ListBuffer[Double]]): (ListBuffer[Double], ListBuffer[Double]) = {
    val lowBound = new ListBuffer[Double]()
    val uppBound = new ListBuffer[Double]()
    data.head.foreach{
      dt =>
        lowBound += dt
        uppBound += dt
    }
    data.tail.foreach {
      case dataRow =>
        (0 to lowBound.size - 1).foreach {
          index =>
            if (lowBound(index) > dataRow(index)) {
              lowBound(index) = dataRow(index)
            }
            if (uppBound(index) < dataRow(index)) {
              uppBound(index) = dataRow(index)
            }
        }
    }
    (lowBound, uppBound)
  }

  private def calculateBoundingBox(lowerBound: ListBuffer[Double], upperBound: ListBuffer[Double]) = {
    val centers = new ListBuffer[Double]()
    val radius = new ListBuffer[Double]()
    var maxRadius = -1.0
    var splitIndex = 0
    (0 to lowerBound.size - 1).foreach {
      index =>
        centers += (lowerBound(index) + upperBound(index)) / 2
        var rad = (upperBound(index) - lowerBound(index)) / 2
        radius += rad
        if (rad > maxRadius) {
          maxRadius = rad
          splitIndex = index
        }
    }

    (centers, radius, maxRadius, splitIndex)
  }

  private def splitNodes(data: ListBuffer[(ListBuffer[Double], Int)], nodeCenters: ListBuffer[Double], splitIndex: Int) = {
    val splitCutoff: Double = nodeCenters(splitIndex)
    val left = new ListBuffer[(ListBuffer[Double], Int)]()
    val right = new ListBuffer[(ListBuffer[Double], Int)]()
    data.foreach {
      case (row, index) =>
        if (row(splitIndex) < splitCutoff) {
          right += ((row, index))
        } else {
          left += ((row, index))
        }
    }
    (left, right)
  }


  private def buildNode(data: List[List[Double]]): (BBDKDNode, List[Int]) = {
    buildNode(data.zipWithIndex.map { case (d, i) => (d.to[ListBuffer], i) }.to[ListBuffer])
  }

  /**
    * Build a k-d tree from the given set of data.
    */
  private def buildNode(zipData: ListBuffer[(ListBuffer[Double], Int)]): (BBDKDNode, List[Int]) = {
    val count = zipData.size
    val (data, indices) = zipData.unzip
    val nodeIndex = indices.head
    val (lowerBound, upperBound) = getLowerAndUpperBound(data)

    val (nodeCenters, nodeRadiuss, maxRadius, splitIndex) = calculateBoundingBox(lowerBound, upperBound)

    // If the max spread is 0, make this a leaf node
    if (maxRadius < 1E-10) {
      val defafultNodeSum = data.head
      val nodeSum = if (data.size > 1) {
        defafultNodeSum.map {
          case sum => sum * data.size
        }
      } else defafultNodeSum

      val node = BBDKDNode(count, nodeIndex, 0.0, nodeCenters, nodeRadiuss, nodeSum)
      (node, Nil)
    } else {

      val (i1Data, i2data) = splitNodes(zipData, nodeCenters, splitIndex)

      val (nodeLower, _) = buildNode(i1Data)
      val (nodeUpper, _) = buildNode(i2data)
      val nodeSum = (nodeLower.sum zip nodeUpper.sum).map {
        case (nlSum, nuSum) => nlSum + nuSum
      }
      val nodeMean = nodeSum.map(_ / zipData.size)
      val nodeCost = getNodeCost(nodeLower, nodeMean) + getNodeCost(nodeUpper, nodeMean)
      val node = BBDKDNode(count, nodeIndex, nodeCost, nodeCenters, nodeRadiuss, nodeSum, Some(nodeLower), Some(nodeUpper))
      (node, Nil)
    }
  }

  private def getNodeCost(node: BBDKDNode, center: ListBuffer[Double]): Double = {
    var total = 0.0

    (0 to center.size - 1).foreach {
      index =>
        val cost = ((node.sum(index) / node.count) - center(index))
        total = total + (cost * cost)
    }

    node.cost + (node.count * total)
  }

  def clustering(centroids: List[List[Double]], sums: List[List[Double]],
                 counts: List[Int], labels: List[Int])(implicit system: ActorSystem): (Double, List[List[Double]], List[Int], List[Int]) = {
    val centroidSize: Int = centroids.length

    val candidates = (0 to centroidSize - 1).toList
    val sumInsideSize = sums(0).size
    val newSums = (1 to centroidSize).toList.map {
      case idx => (1 to sumInsideSize).toList.map(_ => 0.0)
    }

    implicit val timeout = Timeout(60 * 60 seconds)
    val kdRouter = system.actorOf(Props[KDFilterActor])
    val result = (kdRouter ? FilterCluster(root, centroids, candidates, centroidSize, newSums, counts, labels)).map {
      case (totalCost: Double, totalSums: ListBuffer[ListBuffer[Double]], totalCounts: ListBuffer[Int], finalLabels: ListBuffer[Int]) =>
        (totalCost, totalSums.map(_.toList).toList, totalCounts.toList, finalLabels.toList)
    }
    Await.result(result, 2 * 60 * 60 seconds)
  }

}

object BBDKDTree {

  def apply(data: List[List[Double]]) = {
    val n = data.length
    val index = (0 to n - 1).toList
    val emptyNode = BBDKDNode(0)
    val tree = new BBDKDTree(emptyNode, index)
    val (root, treeIndex) = tree.buildNode(data)
    tree.copy(root = root, index = treeIndex)
  }
}

case class BBDKDNode(
                      count: Int,
                      index: Int, //index in the training data/record
                      cost: Double,
                      center: ListBuffer[Double],
                      radius: ListBuffer[Double],
                      sum: ListBuffer[Double],
                      var lower: Option[BBDKDNode] = None,
                      var upper: Option[BBDKDNode] = None
                    )

object BBDKDNode {
  def apply(index: Int) = {
    new BBDKDNode(0, index, 0.0, new ListBuffer[Double](), new ListBuffer[Double](), new ListBuffer[Double]())
  }

}
