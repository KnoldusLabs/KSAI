package ksai.core.classification.knn

import ksai.training.validation.ValidationImplicits
import org.scalatest.{FlatSpec, Matchers, WordSpec}

class KDTreeSpec extends WordSpec with Matchers with ValidationImplicits {

  /*"KDTree" should{

    "build itself" in {

     /* val kdTree = KDTree(
        List(List(1.0, 2.0)),
        List(4.0, 5.0)
      )*/



      val knn: KNN = KNN.learn(x, y, 1)

      assert(true)
    }

  }*/

}
