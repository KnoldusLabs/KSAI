package ksai.core.cluster.linkage

case class SingleLinkage(proximity: Array[Array[Double]]) extends Linkage {

  override def toString: String = "single linkage"

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
