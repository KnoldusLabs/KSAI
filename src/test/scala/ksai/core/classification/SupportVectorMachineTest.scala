package ksai.core.classification

import ksai.data.parser.ARFFParser
import ksai.kernels.LinearKernel
import ksai.training.validation.ValidationImplicits
import org.scalatest.{AsyncFlatSpecLike, Matchers}

class SupportVectorMachineTest extends AsyncFlatSpecLike with Matchers with ValidationImplicits {


  "An SVM" should "just work" in {
    val arffFile = getClass.getResource("/iris.arff").getPath
    val arff = ARFFParser.parse(arffFile)
    val y: Array[Int] = arff.getNumericTargets.toArray //arff.data.map(_.size).toArray
    val maxAttrSize = y.max
    val svm = SupportVectorMachine[Array[Double]](new LinearKernel(), 10.0, maxAttrSize + 1, ONE_VS_ALL)
    val x: Array[Array[Double]] = arff.data.toArray

    println(arff.getNumericTargets)

    for{
      firstSVM <- {
        println("....calling first")
        svm.learn(x, y)
      }
      secondSVM <- {
        println("........calling second")
        println(s"........${firstSVM.k}")
        firstSVM.learn(x, y)
      }
      finishedSVM <- {
        println("...........calling finish")
        secondSVM.finish()
      }
    } yield{
      var index = 0
      val errorCount = x.foldLeft(0){
        case (error, xRow) =>
          println(s"...................................${finishedSVM.predict(xRow)} ${y(index)}")
          val errors = if (finishedSVM.predict(xRow) == y(index)){
            error
          } else {
            error + 1
          }
          index = index + 1
          errors
      }
      println(s"..............................$errorCount")
      assert(errorCount <= 10)
    }

  }
}
