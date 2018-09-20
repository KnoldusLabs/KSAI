package ksai.data.parser

import scala.collection.mutable
import scala.io.Source

/**
  * Parse a file containing delimited data only (no column name or any details)
  * @param responseIndex index where responses are stored (by default it takes last index as response index)
  * @param delimiter can be a whitespace or a comma by which data is delimited
  * @param labelMap map containing the order of responses (to be used while testing data)
  */
class DelimitedParser(responseIndex: Int = -1, delimiter: String = "\\s+", labelMap: mutable.Map[String, Int] = mutable.Map.empty[String, Int]){

  def parse(filename: String): Delimited[String] = {
    val sourceBuffer = Source.fromFile(filename)

    val result = sourceBuffer.getLines.foldLeft(Delimited[String]()) {
      case (delimited, line) =>
        if (line.trim.startsWith("%") || line.trim.equals("")) {
          delimited
        } else {
          val splitData = line.trim.split(delimiter).map(_.trim)
          val data = new Array[Double](splitData.length - 1)

          for(i <- 0 until splitData.length - 1){
            if(i < responseIndex) data(i) = splitData(i).toDouble
            else data(i) = splitData(i + 1).toDouble
          }

          val response = if (responseIndex >= 0) {
            splitData(responseIndex)
          } else {
            splitData.last
          }

          val newData = delimited.data :+ data
          val newTarget = delimited.target :+ response
          delimited.copy(data = newData, target = newTarget)
        }
    }
    sourceBuffer.close()
    val labels: List[String] = result.target.distinct
    labels.zipWithIndex.foreach{
      case (key, value) => labelMap.getOrElseUpdate(key, value)
    }
    result.copy(labels = labels, labelMap = labelMap.toMap)
  }

}
