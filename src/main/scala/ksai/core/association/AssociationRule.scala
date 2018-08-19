package ksai.core.association

case class AssociationRule(
                            /**
                              * Antecedent(LHS) item set.
                              */
                            antecedent: Array[Int],

                            /**
                              * Consequent(RHS) item set.
                              */
                            consequent: Array[Int],

                            /**
                              * Support of the item set.
                              */
                            support: Double,

                            /**
                              * The confidence of the item set.
                              */
                            confidence: Double) {

  override def toString: String = {
    "(" + antecedent.mkString(",") + ")" + " => " + "(" + consequent.mkString(",") + ")" + "\tsupport = " + support * 100 + "\tconfidence = " + confidence * 100
  }
}
