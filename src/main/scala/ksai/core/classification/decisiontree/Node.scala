package ksai.core.classification.decisiontree

import ksai.core.classification.{Attribute, NOMINAL, NUMERIC}

private[decisiontree] case class Node(output: Int = -1,
                                      maybePosteriori: Option[Array[Double]] = None,
                                      var splitFeature: Int = -1,
                                      var splitValue: Double = Double.NaN,
                                      var splitScore: Double = 0.0,
                                      var maybeTrueChild: Option[Node] = None,
                                      var maybeFalseChild: Option[Node] = None,
                                      var trueChildOutput: Int = -1,
                                      var falseChildOutput: Int = -1) {

  def predict(x: Array[Double], attributes: Array[Attribute]): Int = {
    if (maybeTrueChild.isEmpty && maybeFalseChild.isEmpty) {
      output
    } else {
      attributes(splitFeature).`type` match {
        case NOMINAL       =>
          if (x(splitFeature) == splitValue) {
            maybeTrueChild.fold(output)(_.predict(x, attributes))
          } else {
            maybeFalseChild.fold(output)(_.predict(x, attributes))
          }
        case NUMERIC       =>
          if (x(splitFeature) <= splitValue) {
            maybeTrueChild.fold(output)(_.predict(x, attributes))
          } else {
            maybeFalseChild.fold(output)(_.predict(x, attributes))
          }
        case attributeType => throw new IllegalStateException("Unsupported Attribute type: " + attributeType)
      }
    }
  }
}
