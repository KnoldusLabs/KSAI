package ksai.core.classification

import scala.collection.mutable.ArrayBuffer

class NominalAttribute(name: String, values: ArrayBuffer[String]) extends Attribute(NOMINAL, name = name) {

  def size(): Int = values.size
}
