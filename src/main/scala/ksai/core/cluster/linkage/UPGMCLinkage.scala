package ksai.core.cluster.linkage


/**
  * Unweighted Pair Group Method using Centroids (also known as centroid linkage).
  * The distance between two clusters is the Euclidean distance between their
  * centroids, as calculated by arithmetic mean. Only valid for Euclidean
  * distance based proximity matrix.
  *
  */
case class UPGMCLinkage(proximity: Array[Array[Double]], samples: Array[Int]) extends Linkage {

  override def toString = "UPGMC linkage"

  override def merge(firstClusterId: Int, secondClusterId: Int): Unit = {
    val sum = samples(firstClusterId) + samples(secondClusterId)

    (0 until firstClusterId).foreach { itr =>
      proximity(firstClusterId)(itr) =
        (proximity(firstClusterId)(itr) * samples(firstClusterId) + proximity(secondClusterId)(itr) * samples(secondClusterId) - proximity(secondClusterId)(firstClusterId) * samples(firstClusterId) * samples(secondClusterId) / sum) / sum
    }

    ((firstClusterId + 1) until secondClusterId).foreach { itr =>
      proximity(itr)(firstClusterId) =
        (proximity(itr)(firstClusterId) * samples(firstClusterId) + proximity(secondClusterId)(itr) * samples(secondClusterId) - proximity(secondClusterId)(firstClusterId) * samples(firstClusterId) * samples(secondClusterId) / sum) / sum
    }

    ((secondClusterId + 1) until proximity.length).foreach { itr =>
      proximity(itr)(firstClusterId) =
        (proximity(itr)(firstClusterId) * samples(firstClusterId) + proximity(itr)(secondClusterId) * samples(secondClusterId) - proximity(secondClusterId)(firstClusterId) * samples(firstClusterId) * samples(secondClusterId) / sum) / sum
    }

    samples(firstClusterId) = samples(firstClusterId) + samples(secondClusterId)
  }
}

object UPGMCLinkage{
  def apply(innerProximity: Array[Array[Double]]): UPGMCLinkage = {

    val temp = new Array[Int](innerProximity.length)
    temp.indices.foreach { itr =>
      temp(itr) = 1
      (0 until itr).foreach { innerItr =>
        innerProximity(itr)(innerItr) = innerProximity(itr)(innerItr) * innerProximity(itr)(innerItr)
      }
    }
    new UPGMCLinkage(proximity = innerProximity, samples = temp)
  }
}
