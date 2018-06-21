package ksai.core.cluster.linkage

case class WardLinkage(proximity: Array[Array[Double]], samples: Array[Int]) extends Linkage {

  override def toString: String = "Ward's linkage"

  override def merge(firstClusterId: Int, secondClusterId: Int): Unit = {

    val sum = samples(firstClusterId) + samples(secondClusterId)

    (0 until firstClusterId).foreach { itr =>
      proximity(firstClusterId)(itr) =
        (proximity(firstClusterId)(itr) * (samples(firstClusterId) + samples(itr)) + proximity(secondClusterId)(itr) * (samples(secondClusterId) + samples(itr)) - proximity(secondClusterId)(firstClusterId) * samples(itr)) / (sum + samples(itr))
    }

    ((firstClusterId + 1) until secondClusterId).foreach { itr =>
      proximity(itr)(firstClusterId) =
        (proximity(itr)(firstClusterId) * (samples(firstClusterId) + samples(itr)) + proximity(secondClusterId)(itr) * (samples(secondClusterId) + samples(itr)) - proximity(secondClusterId)(firstClusterId) * samples(itr)) / (sum + samples(itr))
    }

    ((secondClusterId + 1) until proximity.length).foreach { itr =>
      proximity(itr)(firstClusterId) =
        (proximity(itr)(firstClusterId) * (samples(firstClusterId) + samples(itr)) + proximity(itr)(secondClusterId) * (samples(secondClusterId) + samples(itr)) - proximity(secondClusterId)(firstClusterId) * samples(itr)) / (sum + samples(itr))
    }

    samples(firstClusterId) = samples(firstClusterId) + samples(secondClusterId)

  }

}

object WardLinkage{
  def apply(innerProximity: Array[Array[Double]]): WardLinkage = {

    val temp = new Array[Int](innerProximity.length)
    temp.indices.foreach { itr =>
      temp(itr) = 1
      (0 until itr).foreach { innerItr =>
        innerProximity(itr)(innerItr) = innerProximity(itr)(innerItr) * innerProximity(itr)(innerItr)
      }
    }
    new WardLinkage(proximity = innerProximity, samples = temp)
  }
}
