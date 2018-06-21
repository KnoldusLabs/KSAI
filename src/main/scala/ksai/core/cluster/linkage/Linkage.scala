package ksai.core.cluster.linkage

trait Linkage {

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
