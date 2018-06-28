package ksai.data.parser

trait Attribute {

  /**
    * Optional weight of this attribute. By default, it is 1.0. The particular
    * meaning of weight depends on applications and machine learning algorithms.
    * Although there are on explicit requirements on the weights, in general,
    * they should be positive.
    */
  def weight: Double

  /**
    * The name of attribute.
    */
  def name: String

  /**
    * The detailed description of the attribute.
    */
  def description: String


}
