package ksai.core.classification

case class ClassifierTrainer[T](attribute: Array[Attribute]) {

  def setAttributes(attribute: Array[Attribute]) = {
    this.copy(attribute = attribute)
  }
}
