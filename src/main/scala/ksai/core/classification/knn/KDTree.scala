package ksai.core.classification.knn


case class Neighbor(
                     key: Array[Double],//The key of neighbor.
                     value: Array[Double],//The data object of neighbor. It may be same as the key object.
                     index: Int,//The index of neighbor object in the dataset.
                     distance: Double//The distance between the query and the neighbor.
                   ) {
  def compareTo(neighbor: Neighbor): Int = {
    val d: Int = Math.signum(distance - neighbor.distance).toInt
    if (d == 0) {
      index - neighbor.index
    } else {
      d
    }
  }
}

case class KDTree(
                   keys: Array[Array[Double]], //Keys of objects
                   data: Array[Array[Double]], //Data objects
                   root: Node,
                   index: Array[Int],
                   identicalExcluded: Boolean = true
                 ) extends KNNSearch {
  def knn(q: Array[Double], k: Int): List[Neighbor] = {

    if (k <= 0) {
      throw new IllegalArgumentException("Invalid k: " + k)
    } else if (k > keys.length) {
      throw new IllegalArgumentException("Neighbor array length is larger than the dataset size")
    }
    val neighbor: Neighbor = Neighbor(Array.fill(0)(0.0), Array.fill[Double](k)(0), 0, Double.MaxValue)
    val heapSelect: HeapSelect = HeapSelect(k = k, n = k, heap = Array.fill(k)(neighbor)) //HeapSelect(k, neigbour)
    heapSelect.heapify(heapSelect.heap)

    //SEARCHING NEIGHBOURS
    search(q, root, heapSelect)
    heapSelect.sort()

    heapSelect.heap.map { neighborI =>
      neighborI.copy(distance = Math.sqrt(neighborI.distance))
    }.toList
  }


  def search(q: Array[Double], node: Node, heapSelect: HeapSelect): Unit = {

    if (node.isLeaf) {
      (node.index until (node.index + node.count)).map { idx =>
        if (q.toList == keys(index(idx)).toList && identicalExcluded) {
          //Skip and do nothing
        } else {
          val distance = squaredDistance(q, keys(index(idx)))
          val datum: Neighbor = heapSelect.heap(0)

          if (distance < datum.distance) {
            val newDatum = Neighbor(keys(index(idx)), data(index(idx)), index(idx), distance)
            heapSelect.updatePeek(newDatum) //Update heap peek element
            heapSelect.heapify()
          }
        }
      }
    } else {
      val diff: Double = q(node.split) - node.cutoff.toInt
      val (nearerOpt, furtherOpt) = if (diff < 0) {
        (node.lower, node.upper)
      } else {
        (node.upper, node.lower)
      }
      //Look in fist half
      nearerOpt.map(nearer => search(q, nearer, heapSelect))

      // Now look in further half
      if (heapSelect.peak().distance >= diff * diff) {
        furtherOpt.map(further => search(q, further, heapSelect))
      }
    }
  }


  //TODO Can be moved to different Math specific module
  def squaredDistance(x: Array[Double], y: Array[Double]): Double = {
    if (x.length != y.length) {
      throw new IllegalArgumentException("Input vector size are different")
    }
    var sum = 0.0
    (0 until x.length).map { i =>
      sum += sqr(x(i) - y(i))
    }
    sum
  }

  //TODO Can be moved to different Math specific module
  def sqr(x: Double): Double = x * x

}

object KDTree {

  def apply(key: Array[Array[Double]], data: Array[Int]): KDTree = {
    val n: Int = key.length
    val index = Range(0, key.size).toArray
    val root: Node = buildNode(key, 0, n, index) //Build KDTree
    new KDTree(
      key,
      key,
      root,
      index
    )
  }

  def buildNode(keys: Array[Array[Double]], begin: Int, end: Int, index: Array[Int]): Node = {
    val d = keys(0).length
    val lowerBound: Array[Double] = Array.fill[Double](d)(0)
    val upperBound: Array[Double] = Array.fill[Double](d)(0)

    (0 until d).map { i =>
      lowerBound.update(i, keys(index(begin))(i))
      upperBound.update(i, keys(index(begin))(i))
    }

    (begin + 1 until end).map { i =>
      (0 until d).map { j =>
        val c = keys(index(i))(j)
        if (lowerBound(j) > c) {
          lowerBound.update(j, c)
        }
        if (upperBound(j) < c) {
          upperBound.update(j, c)
        }
      }
    }

    val nodeInit = Node(count = end - begin, index = begin)

    val (node, newMaxRadius): (Node, Double) = (0 until d).foldLeft((nodeInit, -1.0)) { (nodeAndMaxRadius, i) =>
      val (node, maxRadius) = nodeAndMaxRadius
      val radius = (upperBound(i) - lowerBound(i)) / 2
      if (radius > maxRadius) {
        val split = i //Setting split
        val cutoff = (upperBound(i) + lowerBound(i)) / 2 //Setting cut-off
        (node.copy(split = split, cutoff = cutoff), radius)
      } else {
        (node, maxRadius)
      }
    }

    // If the max spread is 0, make this a leaf node
    if (newMaxRadius == 0 && node.lower == None && node.upper == None) {
      node
    } else {
      // Partition the data set around the midpoint in this dimension. The
      // partitioning is done in-place by iterating from left-to-right and
      // right-to-left in the same way that partitioning is done in quick sort.
      var start = begin
      var finish = end - 1
      var size = 0

      while (start < finish) {
        var i1Good: Boolean = keys(index(start))(node.split) < node.cutoff
        var i2Good: Boolean = keys(index(finish))(node.split) >= node.cutoff
        if (!i1Good && !i2Good) {
          val temp = index(start)
          index(start) = index(finish)
          index(finish) = temp
          i1Good = true
          i2Good = true
        }
        if (i1Good) {
          start += 1
          size += 1
        }
        if (i2Good) finish = finish - 1
      }

      // Create the left child node
      val newLower: Option[Node] = if (begin < end) {
        Some(buildNode(keys, begin, begin + size, index))
      } else {
        None
      }

      // Create the left child node
      val newUpper: Option[Node] = if (begin < end) {
        Some(buildNode(keys, begin + size, end, index))
      } else {
        None
      }

      node.copy(lower = newLower, upper = newUpper)
    }
  }

}

case class Node(
                 count: Int = 0,//Number of dataset stored in this node.
                 index: Int = 0,//The smallest point index stored in this node.
                 split: Int = 0,//The index of coordinate used to split this node.
                 cutoff: Double = 0,//The cutoff used to split the specific coordinate.
                 lower: Option[Node] = None,//The child node which values of split coordinate is less than the cutoff value.
                 upper: Option[Node] = None//The child node which values of split coordinate is greater than or equal to the cutoff value.
               ) {

  def isLeaf: Boolean = lower == None && upper == None

}
