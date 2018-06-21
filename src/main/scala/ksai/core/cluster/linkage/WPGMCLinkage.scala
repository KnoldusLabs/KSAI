package ksai.core.cluster.linkage

case class WPGMCLinkage(proximity: Array[Array[Double]]) extends Linkage {

  override def toString: String = "WPGMC linkage"

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
