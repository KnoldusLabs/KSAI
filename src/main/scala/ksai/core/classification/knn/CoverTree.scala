package ksai.core.classification.knn

import spire.std.DoubleIsNRoot

import scala.collection.mutable.ArrayBuffer


case class CoverTree(
                      data: Array[Array[Double]],
                      distance: Metric,
                      root: Node,
                      base: Double = 1.3,
                    ) extends KNNSearch {

  val invLogBase: Double = 1.0 / Math.log(base)

  val identicalExcluded = true

  override def knn(q: Array[Double], k: Int): List[Neighbor] = ???

}


object CoverTree {

  /**
    * The base of our expansion constant. In other words the 2 in 2^i used
    * in covering tree and separation invariants of a cover tree. In
    * paper it's suggested the separation invariant is relaxed in batch
    * construction.
    **/
  var base = 1.3
  /**
    * if we have base 2 then this can be viewed as 1/ln(2), which can be
    * used later on to do invLogBase*ln(d) instead of ln(d)/ln(2), to get log2(d),
    * in getScale method.
    */
  var invLogBase = 1.0 / Math.log(base)

  def apply(data: Array[Array[Double]]): CoverTree = {

    //new CoverTree(data, new EuclideanDistance())
    ???
  }

  def buildCoverTree(data: Array[Array[Double]], distance: Metric): CoverTree = {
    var pointSet: ArrayBuffer[DistanceSet] = ArrayBuffer.empty[DistanceSet]
    var consumedSet: ArrayBuffer[DistanceSet] = ArrayBuffer.empty[DistanceSet]
    val point = data(0)
    val idx = 0
    var maxDist: Double = -1

    (1 until data.length).map{ i =>
      val set = new DistanceSet(data, i)
      val dist: Double = distance.d(point, data(i))
      set.dist = set.dist += dist
      pointSet += set
      if (dist > maxDist) {
        maxDist = dist
      }
    }

    batchInsert(idx, getScale(maxDist), getScale(maxDist), pointSet, consumedSet)
  }

  def batchInsert(p: Int, maxScale: Int, topScale: Int, pointSet: ArrayBuffer[DistanceSet],
                  consumedSet: ArrayBuffer[DistanceSet]): Node = {

    if(pointSet.isEmpty) {
      newLeaf(p)
    } else {
      val maxDist: Double = max(pointSet) // O(|pointSet|) the max dist in pointSet to point "p".
      val nextScale = Math.min(maxScale - 1, getScale(maxDist))
      if (nextScale == Integer.MIN_VALUE) { // We have points with distance 0. if maxDist is 0.
        val children: ArrayBuffer[CTNode] = ArrayBuffer.empty[CTNode]
        var leaf = newLeaf(p)
        children += leaf
        while (!pointSet.isEmpty) {
          val set = pointSet(pointSet.size - 1)
          pointSet.remove(pointSet.size - 1)
          leaf = newLeaf(set.idx)
          children += leaf
          consumedSet += set
        }

        val node = new CTNode(p) // make a new node out of p and assign it the children.
        node.scale = 100 // A magic number meant to be larger than all scales.
        node.maxDist = 0 // since all points have distance 0 to p
        node.children = children.toArray
        node
      }else{

      }
    }
  }

  def getScale(d: Double): Int = {
    Math.ceil(invLogBase * Math.log(d)).toInt
  }

  def newLeaf(idx: Int): CTNode = {
    new CTNode(idx, 0.0, 0.0, null, 100)
  }

  /**
    * Returns the max distance of the reference point p in current node to
    * it's children nodes.
    * @param v the stack of DistanceNode objects.
    * @return the distance of the furthest child.
    */
  def max(v: Array[DistanceSet]): Double = {
    val max: Double = 0.0
    for (DistanceSet n : v) {
      if (max < n.dist.get(n.dist.size() - 1)) {
        max = n.dist.get(n.dist.size() - 1)
      }
    }
    return max
  }

}

/*case class DistanceSet(idx: Int){

  var distance: List[Double] = List.empty

}*/

case class DistanceSet(
                        var data: Array[Array[Double]],
                        /** The index of the instance represented by this node. */
                        var idx: Int,
                        /**
                          * The last distance is to the current reference point
                          * (potential current parent). The previous ones are
                          * to reference points that were previously looked at
                          * (all potential ancestors).
                          */
                        var dist: ArrayBuffer[Double] = ArrayBuffer.empty
                      ) {
  this(data: Array[Array[Double]], idx: Int)={
    this.data = data
    this.idx = idx
  }

  /**
    * Returns the instance represent by this DistanceNode.
    * @return the instance represented by this node.
    */
  def getObject(): Array[Double] = {
    return data(idx)
  }
}

object DistanceSet{

  def apply(idx: Int): DistanceSet = new DistanceSet(idx)

}


case class CTNode(
               var idx: Int, //Index of the data point in the dataset
               var maxDist: Double, // The maximum distance to any grandchild
               var parentDist: Double, // The distance to the parent node.
               var children: Array[CTNode], // The children of the node.
               var scale: Int // The min i that makes base^i &lt;= maxDist. Essentially, it is an upper bound on the distance to any child.
               ){

  /**
    * Constructor.
    * @param idx the index of the object this node is associated with.
    * @param maxDist the distance of the furthest descendant.
    * @param parentDist the distance of the node to its parent.
    * @param children children of the node in a stack.
    * @param scale the scale/level of the node in the tree.
    */
  this(idx: Int, maxDist: Double, parentDist: Double, children: Array[CTNode], scale: Int) ={
    this.idx = idx
    this.maxDist = maxDist
    this.parentDist = parentDist
    this.children = children
    this.scale = scale
  }

  this(idx: Int)={
    this.idx = idx
    this.maxDist = Int.MaxValue
    this.parentDist = 0.0
    this.children = Array.empty
    this.scale = 0
  }

  /** Returns the instance represented by the node.
    * @return the instance represented by the node.
    */
  /*def getObject() {
    return data(idx)
  }*/

  /** Returns whether if the node is a leaf or not.
    * @return true if the node is a leaf node.
    */
  def isLeaf(): Boolean = {
    children == children.isEmpty
  }
}

object CTNode{

  def apply() = {

  }

}
