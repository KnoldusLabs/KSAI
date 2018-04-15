package ksai.core.classification.knn


case class KNN(
                y: Array[Int], //The labels of training samples.
                k: Int, //The number of neighbors for decision.
                c: Int, //The number of classes
                knn: KDTree //The data structure for nearest neighbor search.
              ) {

  def predict(x: Array[Double], posteriori: Option[Array[Double]] = None): Int = {
    val neighbours: List[Neighbor] = knn.knn(x, k)

    if (k == 1) {
      y(neighbours(0).index)
    } else {
      val count = Array.fill(c)(0)

      (0 until k).map { i =>
        count(y(neighbours(i).index)) = count(y(neighbours(i).index)) + 1
      }

      posteriori.map{ post =>
        (0 until c).map { i =>
          post(i) = count(i) / k
        }
      }

      var max = 0
      var idx = 0
      (0 until c).map { i =>
        if (count(i) > max) {
          max = count(i)
          idx = i
        }
      }
      idx
    }
  }
}

object KNN {

  def learn(data: Array[Array[Double]], y: Array[Int], k: Int): KNN = {
    if (data.length != y.length) {
      throw new IllegalArgumentException(String.format(s"The sizes of X and Y don't match: ${data.length} != ${y.length}"))
    } else if(k < 1){
      throw new IllegalArgumentException(s"Illegal k = $k")
    }

    val knn = KDTree(data, y)
    val classes = y.toSet.size
    new KNN(y, k, classes, knn)
  }

}
