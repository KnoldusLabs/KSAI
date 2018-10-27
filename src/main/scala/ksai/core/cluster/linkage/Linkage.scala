package ksai.core.cluster.linkage

trait Linkage {


  /**
    * The proximity matrix to store the pair-wise distance measure as
    * dissimilarity between clusters. To save space, we only need the
    * lower half of matrix. During the clustering, this matrix will be
    * updated to reflect the dissimilarity of merged clusters.
    */
  val proximity: Array[Array[Double]]

  /**
    * Returns the distance/dissimilarity between two clusters/objects, which
    * are indexed by integers.
    */
  def findDistance(firstPoint: Int, secondPoint: Int): Double = {
    if(firstPoint > secondPoint) proximity(firstPoint)(secondPoint)
    else proximity(secondPoint)(firstPoint)
  }

  /**
    * Merge two clusters into one and update the proximity matrix.
    *
    * @param firstClusterId cluster id.
    * @param secondClusterId cluster id.
    */
  def merge(firstClusterId: Int, secondClusterId: Int):Unit

}
