package ksai.core.association

case class AssociationRule(
                            /**
                              * Antecedent(LHS) itemset.
                              */
                            antecedent: Array[Int],

                            /**
                              * Consequent(RHS) itemset.
                              */
                            consequent: Array[Int],

                            /**
                              * Support of the itemset.
                              */
                            support: Double,

                            /**
                              * The confidence of the itemset.
                              */
                            confidence: Double) {

  override def toString: String = {
    "(" + antecedent.mkString(",") + ")" + " => " + "(" + consequent.mkString(",") + ")" + "\tsupport = " + support * 100 + "\tconfidence = " + confidence * 100
  }
}
