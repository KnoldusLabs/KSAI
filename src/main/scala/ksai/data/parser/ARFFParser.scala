package ksai.data.parser


import scala.io.Source

object ARFFParser {

  def parse(filename: String): ARFF = {
    val sourceBuffer = Source.fromFile(filename)

    val result = sourceBuffer.getLines.foldLeft(ARFF()){
      case (arff, line) =>
        if(line.trim.startsWith("%") || line.trim.equals("")){
          arff
        } else {
          if(line.trim.startsWith("@RELATION")){
            arff.copy(relation = line.trim.split(" ").filterNot(token =>
              token.startsWith("@RELATION")).head.trim)
          } else {
            if(line.trim.startsWith("@ATTRIBUTE")) {
               val Array(name, attrType) = line.trim.split(" ").filterNot(token =>
                 token.startsWith("@ATTRIBUTE")).flatMap(token => token.split("\t").filterNot(tok => tok.trim.equals("")))

              if(attrType.trim.startsWith("{") && attrType.trim.endsWith("}")){
                val nominals = attrType.trim.drop(1).dropRight(1).split(",").map(_.trim).toList
                arff.copy(nominals = nominals)
              } else {
                val newAttributeList = arff.attributes :+ AttributeMeta(name, attrType)
                arff.copy(attributes = newAttributeList)
              }
            } else {
                if(line.trim.startsWith("@DATA")){
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
