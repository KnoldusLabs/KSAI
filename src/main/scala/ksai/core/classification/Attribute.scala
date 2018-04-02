package ksai.core.classification


sealed trait Type

/**
  * Numeric attribute.
  */
case object NUMERIC extends Type

/**
  * Nominal attribute. Variables assessed on a nominal scale are called
  * categorical variables.
  */
case object NOMINAL extends Type

/**
  * String attribute. Note that strings should not be treated as nominal
  * attribute because one may use some part of strings (e.g. suffix or
  * prefix) instead of the original string as features.
  */
case object STRING extends Type

/**
  * Date attribute. A date type also optionally specifies the format
  * in which the date is presented, with the default being in ISO-8601 format.
  */
case object DATE extends Type

case class Attribute(
                      `type`: Type,
                      weight:Double = 1.0,
                      name:String,
                      description:String = ""
                    ) extends Serializable {

  override def toString: String = {
    s"${`type`} [$name] "
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case attribute:Attribute =>
        name.equals(attribute.name) && `type` == attribute.`type` && description.equals(attribute.description)
      case _ => false
    }
  }

  override def hashCode(): Int = {
    var hash:Int = 5
    hash = (37 * hash) + `type`.hashCode()
    hash = (37 * hash) + (if(name != "") name.hashCode() else 0)
    hash = (37 * hash) + (if(description != "")description.hashCode() else 0)
    hash
  }
}

