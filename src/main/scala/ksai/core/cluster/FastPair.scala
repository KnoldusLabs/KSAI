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
      distance(point) = if (point > neighbor(point)) proximity(points(point))(points(neighbor(point)))
      else proximity(points(neighbor(point)))(points(point))

      // now test whether each other point is closer
      (first + 1 until npoints).foreach { itr =>
        val q = points(itr)
        if (q != point) {
          val d = if (point > q) proximity(points(point))(points(q)) else proximity(points(q))(points(point))

          if (d < distance(q)) {
            distance(point) = d
            neighbor(point) = q
          }
        }
      }

    }
  }

  /**
    *  Add a point and find its nearest neighbor.
    */
  def add(point: Int) = {
    findNeighbor(point)
    index(point) = npoints + 1
    points(index(point)) = point
  }

  /**
    * Remove a point and update neighbors of points for which it had been nearest
    */
  def remove(point: Int) = {
    copy(npoints = npoints - 1)
    val q = index(point)
    points(q) = points(npoints)
    index(point(q)) = q

    (0 until npoints).foreach{itr =>
      if(neighbor(points(itr)) == point){
        findNeighbor(points(itr))
      }
    }
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
