package ksai.core.cluster.linkage

case class WPGMALinkage(proximity: Array[Array[Double]]) extends Linkage {

  override def toString: String = "WPGMA linkage"

  override def merge(firstClusterId: Int, secondClusterId: Int): Unit = {

    (0 until firstClusterId).foreach{ itr =>
      proximity(firstClusterId)(itr) = (proximity(firstClusterId)(itr) + findDistance(secondClusterId, itr)) / 2
    }

    ((firstClusterId + 1) until proximity.length).foreach{ itr =>
      proximity(itr)(firstClusterId) = (proximity(itr)(firstClusterId) + findDistance(secondClusterId, itr)) / 2
    }
  }

}
