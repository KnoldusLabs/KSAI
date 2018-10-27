package ksai.core.cluster.linkage

/**
  * Single linkage. The distance between groups is defined as the distance
  * between the closest pair of objects, one from each group.
  * A drawback of this method is the so-called chaining phenomenon: clusters
  * may be forced together due to single elements being close to each other,
  * even though many of the elements in each cluster may be very distant to
  * each other.
  * <p>
  * Single linkage clustering is essentially the same as Kruskal's algorithm
  * for minimum spanning trees. However, in single linkage clustering, the
  * order in which clusters are formed is important, while for minimum spanning
  * trees what matters is the set of pairs of points that form distances chosen
  * by the algorithm.
  *
  */

case class SingleLinkage(proximity: Array[Array[Double]]) extends Linkage {

  override def toString = "single linkage"

  override def merge(firstClusterId: Int, secondClusterId: Int): Unit = {
    (0 until firstClusterId).foreach { itr =>
      proximity(firstClusterId)(itr) = Math.min(proximity(firstClusterId)(itr),
        findDistance(secondClusterId, itr))
    }

    ((firstClusterId + 1) until proximity.length).foreach { itr =>
      proximity(itr)(firstClusterId) = Math.min(proximity(itr)(firstClusterId),
        findDistance(secondClusterId, itr))
    }

  }
}
