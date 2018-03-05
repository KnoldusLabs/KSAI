package ksai.core.classification


case class KNN(
                knnSearch: KNNSearch,
                y: Array[Int],
                k: Int,
                c: Int
              ){

}


case class KNNSearch(
                      knn: Neighbor
                    )


case class Neighbor(
                     key: String,
                     value: Double,
                     index: Int,
                     distance: Double
                   ) {


  def compareTo(o: Neighbor) = {
    val d = Math.signum(distance - o.distance).toInt
    if (d == 0) {
      index - o.index
    } else {
      d
    }
  }

}

case class ClassifierTrainer(atribute: Attribute)

case class Attribute(
                      `type`: Type.type,
                      weight: Double,
                      name: String,
                      description: Option[String]
                    ) {

  def hasCode() = {
    var hash: Int = 5
    hash = (37 * hash + `type`.hashCode())
    hash = 37 * hash + (name.hashCode)
    hash = 37 * hash + (if (description.isDefined) {
      description.hashCode()
    } else {
      0
    })
  }

  def equal(obj: Object) = {
    obj match {
      case o: Attribute =>
        if (name.equals(o.name) && `type` == o.`type`) {
          if (description.isDefined && o.description.isDefined && description.eq(o.description)) {
            true
          } else {
            false
          }
        } else {
          false
        }
      case _ => false
    }
  }
}


object Attribute {

  def apply(`type`: Type.type, name: String) = {
    Attribute(`type`, 1.0, name, None)
  }

  def apply(attType: Type.type, attName: String, attWeight: Double) = {
    Attribute(attType, attWeight, attName, None)
  }

}

object Type extends Enumeration {
  val NUMERIC, NOMINAL, STRING, DATE = Value
}


