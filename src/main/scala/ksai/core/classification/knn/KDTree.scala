package ksai.core.classification.knn


case class Neighbor(
                     key: Array[Double],
                     value: Array[Double],
                     index: Int,
                     distance: Double
                   ) {
  def compareTo(neigbour: Neighbor): Int = {
    val d: Int = Math.signum(distance - neigbour.distance).toInt
    if (d == 0) {
      index - neigbour.index
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
                 ) {
  def knn(q: Array[Double], k: Int): List[Neighbor] = {

    if (k <= 0) throw new IllegalArgumentException("Invalid k: " + k)

    if (k > keys.length) throw new IllegalArgumentException("Neighbor array length is larger than the dataset size")

    val neigbour: Neighbor = Neighbor(Array.fill(0)(0.0), Array.fill[Double](k)(0), 0, Double.MaxValue)

    val heapSelect: HeapSelect = HeapSelect(k = k, n = k, heap = Array.fill(k)(neigbour))//HeapSelect(k, neigbour)
    heapSelect.heapify(heapSelect.heap)

    println("*****HEAP BEFORE SEARCH..." + heapSelect.heap.toList)
    //SEARCHING NEIGHBOURS
    search(q, root, heapSelect)
    println("*****HEAP AFTER SEARCH..." + heapSelect.heap.toList)

    heapSelect.sort()

    val res = heapSelect.heap.map{ neightbour =>
      neightbour.copy(distance = Math.sqrt(neightbour.distance))
    }

    println("\n\n\n\n--------New neighbours to be retruned.........>>>>>>>>>>>>>" + res.toList)

    res.toList
  }


  def search(q: Array[Double], node: Node, heapSelect: HeapSelect): Unit = {

    if (node.isLeaf) {
      (node.index until node.index + node.count).map { idx =>
        if (q == keys(index(idx)) && identicalExcluded) {
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

object KDTree extends KNNSearch {

  def apply(key: Array[Array[Double]], data: Array[Int]): KDTree = {
    val n: Int = key.length
    val index = Range(0, key.size).toArray
    val root: Node = buildNode(key, 0, n, index)
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

    var node = Node(count = end - begin, index = begin)

    val (newNode, newMaxRadious): (Node, Double) = (0 until d).foldLeft((node, -1.0)) { (nodeAndmaxRadious, i) =>
      val (node, maxRadious) = nodeAndmaxRadious
      val radious = (upperBound(i) - lowerBound(i)) / 2
      if (radious > maxRadious) {
        val split = i
        val cutoff = (upperBound(i) + lowerBound(i)) / 2
        (node.copy(split = split, cutoff = cutoff), radious)
      } else {
        (node, maxRadious)
      }
    }

    val newNode1 = if (newMaxRadious == 0) {
      newNode.copy(upper = None, lower = None)
    } else {
      newNode
    }

    // If the max spread is 0, make this a leaf node
    if (newMaxRadious == 0 && node.lower == None && node.upper == None) {
      node
    } else {
      // Partition the dataset around the midpoint in this dimension. The
      // partitioning is done in-place by iterating from left-to-right and
      // right-to-left in the same way that partioning is done in quicksort.
      var start = begin
      var finish = end - 1
      var size = 0

      while (start < finish) {
        var i1Good: Boolean = keys(index(start))(newNode1.split) < newNode1.cutoff
        var i2Good: Boolean = keys(index(finish))(newNode1.split) >= newNode1.cutoff

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

      // Create the child nodes
      val newLower: Option[Node] = if (begin < end) {
        Some(buildNode(keys, begin, begin + size, index))
      } else {
        None
      }

      val newUpper: Option[Node] = if (begin < end) {
        Some(buildNode(keys, begin + size, end, index))
      } else {
        None
      }

      newNode1.copy(lower = newLower, upper = newUpper)
    }
  }

}

case class Node(
                 count: Int = 0,
                 index: Int = 0,
                 split: Int = 0,
                 cutoff: Double = 0,
                 lower: Option[Node] = None,
                 upper: Option[Node] = None
               ) {

  def isLeaf: Boolean = lower == None && upper == None

}
