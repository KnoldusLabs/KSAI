package ksai.core.cluster.linkage


/**
  * Unweighted Pair Group Method with Arithmetic mean (also known as average linkage).
  * The distance between two clusters is the mean distance between all possible
  * pairs of nodes in the two clusters.
  * <p>
  * In bioinformatics, UPGMA is used for the creation of phenetic trees
  * (phenograms). UPGMA assumes a constant rate of evolution (molecular
  * clock hypothesis), and is not a well-regarded method for inferring
  * relationships unless this assumption has been tested and justified
  * for the data set being used.
  */
case class UPGMALinkage(proximity: Array[Array[Double]], samples: Array[Int]) extends Linkage {

  override def toString = "UPGMA linkage"

  override def merge(firstClusterId: Int, secondClusterId: Int): Unit = {
    val sum: Double = samples(firstClusterId) + samples(secondClusterId)
    (0 until firstClusterId).foreach { itr =>
      proximity(firstClusterId)(itr) = proximity(firstClusterId)(itr) * samples(firstClusterId) / sum + findDistance(secondClusterId, itr) * samples(secondClusterId) / sum
    }

    ((firstClusterId + 1) until proximity.length).foreach { itr =>
      proximity(itr)(firstClusterId) = proximity(itr)(firstClusterId) * samples(firstClusterId) / sum + findDistance(secondClusterId, itr) * samples(secondClusterId) / sum
    }

    samples(firstClusterId) = samples(firstClusterId) + samples(secondClusterId)
  }


}

object UPGMALinkage {
  def apply(proximity: Array[Array[Double]]): UPGMALinkage = {
    val temp = new Array[Int](proximity.length)
    temp.indices.foreach { itr =>
      temp(itr) = 1
    }
    new UPGMALinkage(proximity = proximity, samples = temp)
  }
}
