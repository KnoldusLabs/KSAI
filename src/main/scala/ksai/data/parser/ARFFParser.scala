package ksai.data.parser


import scala.io.Source

object ARFFParser {

  def parse(filename: String): ARFF[String] = {
    val sourceBuffer = Source.fromFile(filename)

    val result = sourceBuffer.getLines.foldLeft(ARFF[String]()){
      case (arff, line) =>
        if(line.trim.startsWith("%") || line.trim.equals("")){
          arff
        } else {
          if(line.trim.toUpperCase.startsWith("@RELATION")){
            arff.copy(relation = line.trim.split(" ").filterNot(token =>
              token.toUpperCase.startsWith("@RELATION")).head.trim)
          } else {
            if(line.trim.toUpperCase.startsWith("@ATTRIBUTE")) {
               val Array(name, attrType) = line.trim.split(" ").filterNot{token =>
                 token.toUpperCase.startsWith("@ATTRIBUTE")}.flatMap(token => token.split("\t").filterNot(tok => tok.trim.equals("")))

              if(attrType.trim.startsWith("{") && attrType.trim.endsWith("}")){
                val nominals = attrType.trim.drop(1).dropRight(1).split(",").map(_.trim).toList
                arff.copy(labels = nominals)
              } else {
                val newAttributeList = arff.attributes :+ AttributeMeta(name, attrType)
                arff.copy(attributes = newAttributeList)
              }
            } else {
                if(line.trim.toUpperCase.startsWith("@DATA")){
                 arff.copy(isDataPresent = true)
                } else if(arff.isDataPresent) {
                val splittedData = line.trim.split(",").map(_.trim)
                val newData = arff.data :+ splittedData.dropRight(1).map(_.toDouble)
                val newTarget = arff.target :+ splittedData.last
                arff.copy(data = newData, target = newTarget)
              } else {
                throw new IllegalArgumentException("The Data in ARFF presents are not well formatted")
              }
            }
          }
        }
    }
    sourceBuffer.close()
    result
  }

}
