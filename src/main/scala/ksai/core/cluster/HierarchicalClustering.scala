package ksai.core.cluster

import ksai.core.cluster.linkage.Linkage

case class HierarchicalClustering(
                                   serialVersionUID: Long = 1L,
                                   merge: Array[Array[Int]],
                                   height: Array[Double]
                                 )

object HierarchicalClustering {

  def apply(linkage:Linkage): HierarchicalClustering = {

    val proximity = linkage.proximity

    val n = proximity.length

    val merge = Array.ofDim[Int](n-1, 2)

    val id = new Array[Int](n)

    val height = new Array[Double](n-1)

    val points = new Array[Int](n)

    (0 until n).foreach{ itr =>
      points(itr) = itr
      id(itr) = itr
    }


    ???
  }

}
