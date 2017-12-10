package ksai.training.validation

import ksai.data.parser.ARFF

trait ValidationImplicits {

  implicit class KFold(arff: ARFF[String]) {
    def kFoldNN(k: Int)(trainFunc: (List[Array[Double]], List[Int], List[Array[Double]], List[Int]) => Any) = {
      val validationData = findFoldPoints(k, arff.data.size)
      validationData.map{
        case (trainingSet, validationSet) =>
          trainFunc(trainingSet.map(arff.data(_)), trainingSet.map(arff.getNumericTargets(_)),
            validationSet.map(arff.data(_)), validationSet.map(arff.getNumericTargets(_)))
      }
    }

    private def findFoldPoints(k: Int, dataSize: Int): List[(List[Int], List[Int])] = {
      val dataIndices = (0 to dataSize - 1).toList
      val avgChunkSize = dataSize / k
      val validationChunk = (dataSize % k) + avgChunkSize
      val validationPoints = (1 to k-1).foldLeft(List((0, validationChunk))) {
        case (result, _) =>
          result :+ (result.last._1 + avgChunkSize, result.last._1 + avgChunkSize + validationChunk)
      }
      validationPoints.map{
        case (startIndex, endIndex) =>
          val (testDataIndices, validationDataIndices) = dataIndices.partition {
            index => index < startIndex || index >= endIndex
          }
          (testDataIndices, validationDataIndices)
      }
    }

    def getBinaryTargets(targets: Array[Int], indexToBeTargeted: Int) = {
          if(indexToBeTargeted < 0 || indexToBeTargeted >= targets.length)
            throw new IllegalArgumentException(s"Targeted index cannot be $indexToBeTargeted")

          targets.map(trgt => if(trgt == targets(indexToBeTargeted)) 1 else 0)
    }
  }

}
