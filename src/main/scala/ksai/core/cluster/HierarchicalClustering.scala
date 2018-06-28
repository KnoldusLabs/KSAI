package ksai.core.cluster

import ksai.core.cluster.linkage.{Linkage, UPGMCLinkage, WPGMCLinkage, WardLinkage}
import ksai.sort.IntHeapSelect

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

case class HierarchicalClustering(
                                   serialVersionUID: Long = 1L,
                                   merge: Array[Array[Int]],
                                   height: Array[Double]
                                 ) {
  def partition(clusters: Int): Array[Int] = {

    val n = merge.length + 1
    val membership = new Array[Int](n)
    val heap = IntHeapSelect(clusters)
    (2 to clusters).foreach { itr =>
      heap.add(merge(n - itr)(0))
      heap.add(merge(n - itr)(1))
    }

    (0 until clusters).foreach { itr =>
      bfs(membership, heap.get(itr), itr)
    }

    membership
  }

  def partition(cutHeight: Double): Array[Int] = {
    (0 until (height.length - 1)).foreach { itr =>
      if (height(itr) > height(itr + 1)) new IllegalStateException("Non-monotonic cluster tree -- the linkage is probably not appropriate!")
    }

    val limit = merge.length + 1
    val k = setValue(2, limit, cutHeight)

    if (k <= 2) new IllegalArgumentException("The parameter h is too large.")

    partition(k - 1)

  }

  def bfs(membership: Array[Int], cluster: Int, id: Int): Unit = {

    val n = merge.length + 1
    val queue = mutable.Queue.empty[Int]

    queue.enqueue(cluster)

    def inner: Unit = {
      Try(queue.dequeue).toOption match {
        case None =>
        case Some(i) if i < n =>
          membership(i) = id
          inner
        case Some(i) =>
          val newI = i - n
          val m1 = merge(newI)(0)

          if (m1 >= n) {
            queue.enqueue(m1)
          } else {
            membership(m1) = id
          }

          val m2 = merge(newI)(1)

          if (m2 >= n) {
            queue.enqueue(m2)
          } else {
            membership(m2) = id
          }
          inner
      }
    }


  }

  @tailrec
  private def setValue(value: Int, limit: Int, cutHeight: Double): Int = {
    if (value <= limit) {
      if (height(limit - value) < cutHeight) {
        value
      } else {
        setValue(value + 1, limit, cutHeight)
      }
    } else value
  }
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
