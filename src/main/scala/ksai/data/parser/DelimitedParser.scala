package ksai.data.parser

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
