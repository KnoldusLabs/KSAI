package ksai.data.parser

import scala.collection.mutable
import scala.io.Source

object DelimitedParser {

  def parse(filename: String): Delimited[String] = {
    val sourceBuffer = Source.fromFile(filename)

    val result = sourceBuffer.getLines.foldLeft(Delimited[String]()) {
      case (delimited, line) =>
        if (line.trim.startsWith("%") || line.trim.equals("")) {
          delimited
        } else {
          val splittedData = line.trim.split("\\s+").map(_.trim)
          val newData = delimited.data :+ splittedData.dropRight(1).map(_.toDouble)
          val newTarget = delimited.target :+ splittedData.last
          delimited.copy(data = newData, target = newTarget)
        }
    }
    sourceBuffer.close()
    result.copy(labels = result.target.distinct)
  }

  def parseZip(filename: String): Delimited[String] = {
    val sourceBuffer = Source.fromFile(filename)

    val result = sourceBuffer.getLines.foldLeft(Delimited[String]()) {
      case (delimited, line) =>
        if (line.trim.startsWith("%") || line.trim.equals("")) {
          delimited
        } else {
          val splittedData = line.trim.split("\\s+").map(_.trim)
          val newData = delimited.data :+ splittedData.drop(1).map(_.toDouble)
          val newTarget = delimited.target :+ splittedData.head
          delimited.copy(data = newData, target = newTarget)
        }
    }
    sourceBuffer.close()
    result.copy(labels = result.target.distinct)
  }

}

class DelimitedParserRefactored(responseIndex: Int, delimiter: String = "\\s+", labelMap: mutable.Map[String, Int] = mutable.Map.empty[String, Int]){

  def parse(filename: String): DelimitedRefactored[String] = {
    val sourceBuffer = Source.fromFile(filename)

    val result = sourceBuffer.getLines.foldLeft(DelimitedRefactored[String]()) {
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
          val newData = delimited.data :+ data
          val newTarget = delimited.target :+ splitData(responseIndex)
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
