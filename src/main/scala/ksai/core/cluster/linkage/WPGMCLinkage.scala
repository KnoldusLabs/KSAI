package ksai.core.cluster.linkage


/**
  * Weighted Pair Group Method using Centroids (also known as median linkage).
  * The distance between two clusters is the Euclidean distance between their
  * weighted centroids. Only valid for Euclidean distance based proximity matrix.
  *
  */
case class WPGMCLinkage(proximity: Array[Array[Double]]) extends Linkage {

  override def toString = "WPGMC linkage"

  override def merge(firstClusterId: Int, secondClusterId: Int): Unit = {

    (0 until firstClusterId).foreach{ itr =>
      proximity(firstClusterId)(itr) = (proximity(firstClusterId)(itr) +  proximity(secondClusterId)(itr)) / 2 - proximity(secondClusterId)(firstClusterId) / 4
    }

    ((firstClusterId + 1) until secondClusterId).foreach{ itr =>
      proximity(itr)(firstClusterId) = (proximity(itr)(firstClusterId) +  proximity(secondClusterId)(itr)) / 2 - proximity(secondClusterId)(firstClusterId) / 4
    }

    ((secondClusterId + 1) until proximity.length).foreach{ itr =>
      proximity(itr)(firstClusterId) = (proximity(itr)(firstClusterId) +  proximity(itr)(secondClusterId)) / 2 - proximity(secondClusterId)(firstClusterId) / 4
    }
  }

}

object WPGMCLinkage{
  def apply(innerProximity: Array[Array[Double]]): WPGMCLinkage = {

    innerProximity.indices.foreach{ itr =>
      (0 until itr).foreach{ innerItr =>
        innerProximity(itr)(innerItr) = innerProximity(itr)(innerItr) * innerProximity(itr)(innerItr)
      }
    }

    new WPGMCLinkage(proximity = innerProximity)
  }
}
