package ksai.core.cluster.linkage

case class CompleteLinkage(proximity: Array[Array[Double]]) extends Linkage {

  override def merge(firstClusterId: Int, secondClusterId: Int): Unit = {
    (0 until firstClusterId).foreach { itr =>
      proximity(firstClusterId)(itr) = Math.max(proximity(firstClusterId)(itr),
        findDistance(secondClusterId, itr))
    }

    ((firstClusterId + 1) until proximity.length).foreach { itr =>
      proximity(itr)(firstClusterId) = Math.max(proximity(itr)(firstClusterId),
        findDistance(secondClusterId, itr))
    }

  }

  override def toString: String = "complete linkage"
}
