package ksai.core.cluster.linkage


/**
  * Weighted Pair Group Method with Arithmetic mean. WPGMA down-weights the
  * largest group by giving equal weights to the two branches of the dendrogram
  * that are about to fuse.
  * <p>
  * Note that the terms weighted and unweighted refer to the final result,
  * not the math by which it is achieved. Thus the simple averaging in WPGMA
  * produces a weighted result, and the proportional averaging in UPGMA produces
  * an unweighted result.
  *
  */
case class WPGMALinkage(proximity: Array[Array[Double]]) extends Linkage {

  override def toString = "WPGMA linkage"

  override def merge(firstClusterId: Int, secondClusterId: Int): Unit = {

    (0 until firstClusterId).foreach{ itr =>
      proximity(firstClusterId)(itr) = (proximity(firstClusterId)(itr) + findDistance(secondClusterId, itr)) / 2
    }

    ((firstClusterId + 1) until proximity.length).foreach{ itr =>
      proximity(itr)(firstClusterId) = (proximity(itr)(firstClusterId) + findDistance(secondClusterId, itr)) / 2
    }
  }

}
