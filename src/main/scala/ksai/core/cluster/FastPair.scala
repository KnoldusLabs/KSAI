package ksai.core.cluster

case class FastPair(
                     points: Array[Int], // points currently in set
                     index: Array[Int], // indices into points
                     npoints: Int, // how much of array is actually used?
                     neighbor: Array[Int],
                     distance: Array[Double],
                     proximity: Array[Array[Double]]
                   ) {

  /**
    * Add a point and find its nearest neighbor.
    */
  def add(point: Int) = {
    findNeighbor(point)
    index(point) = npoints + 1
    points(npoints + 1) = point
  }

  /**
    * Remove a point and update neighbors of points for which it had been nearest
    */
  def remove(point: Int) = {
    this.copy(npoints = npoints - 1)
    val q = index(point)
    points(q) = points(npoints - 1)
    index(points(q)) = q

    (0 until npoints - 1).foreach { itr =>
      if (neighbor(points(itr)) == point) {
        findNeighbor(points(itr))
      }
    }
  }

  /**
    * Find closest pair by scanning list of nearest neighbors
    */
  def getNearestPair(pair: Array[Int]): Double = {
    if (npoints < 2) throw new IllegalStateException("FastPair: not enough points to form pair")

    var calculatedDistance = distance(points(0))
    var r = 0
    (1 until npoints).foreach { itr =>
      if (distance(points(itr)) < calculatedDistance) {
        calculatedDistance = distance(points(itr))
        r = itr
      }
    }

    pair(0) = points(r)
    pair(1) = neighbor(pair(0))

    if (pair(0) > pair(1)) {
      val temp = pair(0)
      pair(0) = pair(1)
      pair(1) = temp
    }

    calculatedDistance
  }

  /**
    * All distances to point have changed, check if our structures are ok
    * Note that although we completely recompute the neighbors of p,
    * we don't explicitly call findNeighbor, since that would double
    * the number of distance computations made by this routine.
    * Also, like deletion, we don't change any other point's neighbor to p.
    */
  def updatePoint(p: Int): Unit = {
    neighbor(p) = p // flag for not yet found any
    distance(p) = Double.MaxValue
    (0 until npoints).foreach { itr =>
      val q = points(itr)
      if (q != p) {
        val d = findDistance(p, q)
        if (d < distance(p)) {
          distance(p) = d
          neighbor(p) = q
        }
        if (neighbor(q) == p) if (d > distance(q)) findNeighbor(q) else distance(q) = d
      }

    }
  }

  def updateDistance(p: Int, q: Int) = {
    val d = findDistance(p, q)
    if (d < distance(p)) {
      distance(p) = q
      neighbor(p) = q
    } else if (neighbor(p) == q && d > distance(p)) findNeighbor(p)

    if (d < distance(q)) {
      distance(q) = p
      neighbor(q) = p
    } else if (neighbor(q) == p && d > distance(q)) findNeighbor(q)
  }

  /**
    * Find nearest neighbor of a given point.
    */
  def findNeighbor(point: Int): Unit = {

    // if no neighbors available, set flag for UpdatePoint to find
    if (npoints == 1) {
      neighbor(point) = point
      distance(point) = Double.MaxValue
    } else {

      // find first point unequal to point itself
      var first = 0
      if (point == points(first)) first = 1

      neighbor(point) = points(first)
      distance(point) = findDistance(point, neighbor(point))

      // now test whether each other point is closer
      (first + 1 until npoints).foreach { itr =>
        val q = points(itr)
        if (q != point) {
          val d = findDistance(point, q)

          if (d < distance(q)) {
            distance(point) = d
            neighbor(point) = q
          }
        }
      }

    }
  }

  /**
    * Returns the distance/dissimilarity between two clusters/objects, which
    * are indexed by integers.
    */
  def findDistance(first: Int, second: Int): Double = {
    if (first > second) proximity(first)(second)
    else proximity(second)(first)
  }

}

object FastPair {

  def apply(pointsn: Array[Int], proximity: Array[Array[Double]]): FastPair = {

    val npoints = pointsn.length
    val neighbor = new Array[Int](npoints)
    val index = new Array[Int](npoints)
    val distance = new Array[Double](npoints)
    val points = pointsn

    // Find all neighbors. We use a conga line rather than calling getNeighbor.
    (0 until npoints - 1).foreach { itr =>

      // find neighbor to p[0]
      var nbr = itr + 1
      var nbd = Double.MaxValue
      (itr + 1 until npoints).foreach { innerItr =>

        //the distance/dissimilarity between two clusters/objects, which
        // are indexed by integers.
        val d = if (points(itr) > points(innerItr)) {
          proximity(points(itr))(points(innerItr))
        } else {
          proximity(points(innerItr))(points(itr))
        }

        if (d < nbd) {
          nbr = innerItr
          nbd = d
        }
      }

      // add that edge, move nbr to points[i+1]
      distance(points(itr)) = nbd
      neighbor(points(itr)) = points(nbr)
      points(nbr) = points(itr + 1)
      points(itr + 1) = neighbor(points(itr))

    }

    // No more neighbors, terminate conga line
    neighbor(points(npoints - 1)) = points(npoints - 1)
    distance(points(npoints - 1)) = Double.MaxValue

    // set where_are...
    (0 until npoints).foreach { itr =>
      index(points(itr)) = itr
    }

    new FastPair(
      points = pointsn,
      index = index,
      npoints = npoints,
      neighbor = neighbor,
      distance = distance,
      proximity = proximity
    )
  }

}
