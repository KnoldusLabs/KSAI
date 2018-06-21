package ksai.core.cluster

import ksai.core.cluster.linkage.{Linkage, UPGMCLinkage, WPGMCLinkage, WardLinkage}

case class HierarchicalClustering(
                                   serialVersionUID: Long = 1L,
                                   merge: Array[Array[Int]],
                                   height: Array[Double]
                                 ){
  /*def partition(clusters: Int):Int = {

  }*/
}

object HierarchicalClustering {

  def apply(linkage: Linkage): HierarchicalClustering = {

    val proximity = linkage.proximity
    val n = proximity.length
    val innerMerge = Array.ofDim[Int](n - 1, 2)
    val id = new Array[Int](n)
    val innerHeight = new Array[Double](n - 1)
    val points = new Array[Int](n)

    (0 until n).foreach { itr =>
      points(itr) = itr
      id(itr) = itr
    }

    val fastPair = FastPair(points, proximity)
    (0 until n - 1).foreach { itr =>
      innerHeight(itr) = fastPair.getNearestPair(innerMerge(itr))
      linkage.merge(innerMerge(itr)(0), innerMerge(itr)(1)) // innerMerge clusters into one
      fastPair.remove(innerMerge(itr)(1)) // drop b
      fastPair.updatePoint(innerMerge(itr)(0)) // and tell closest pairs about merger

      val p = innerMerge(itr)(0)
      val q = innerMerge(itr)(1)

      innerMerge(itr)(0) = Math.min(id(p), id(q))
      innerMerge(itr)(1) = Math.max(id(p), id(q))
      id(p) = n + itr
    }

    if (linkage.isInstanceOf[UPGMCLinkage] || linkage.isInstanceOf[WPGMCLinkage] || linkage.isInstanceOf[WardLinkage]) {
      innerHeight.indices.foreach { itr =>
        innerHeight(itr) = Math.sqrt(innerHeight(itr))
      }
    }

    new HierarchicalClustering(merge = innerMerge, height = innerHeight)
  }

}
