package ksai.core.association.fptree

trait FPGrowthMagnet {

  type Result = FPGrowth

  def apply(): Result
}

object FPGrowthMagnet {

  /**
    * Constructor for FPGrowth. This is a two step construction where user will
    * first scan the database to get the frequency of single items and then call
    * [[FPGrowth.add()]] to add itemsets into the FP-Tree during the second
    * scan.
    *
    * @param tuple (frequencies of single items, required minimum support of
    *              itemsets in terms of frequency).
    */
  implicit def forFrequency(tuple: (Array[Int], Int)) = new FPGrowthMagnet {

    def apply(): Result = {
      val (frequency, minSupport) = (tuple._1, tuple._2)
      new FPGrowth(minSupport, FPTree(frequency, minSupport))
    }
  }

  /**
    * Constructor for FPGrowth. This is a one step construction of FP-Tree.
    *
    * @param tuple (itemsets where individual itemset should not contain
    *              duplicate items, required minimum support of itemset
    *              in terms of frequency).
    */
  implicit def forIntegerMinSupport(tuple: (Array[Array[Int]], Int)) = new FPGrowthMagnet {

    def apply(): Result = {
      val (itemsets, minSupport) = (tuple._1, tuple._2)
      new FPGrowth(minSupport, FPTree(itemsets, minSupport))
    }
  }

  /**
    * Constructor for FPGrowth. This is a one step construction of FP-Tree.
    *
    * @param tuple (itemsets where individual itemset should not contain
    *              duplicate items, required minimum support of itemset
    *              in terms of percentage).
    */
  implicit def forPercentageMinSupport(tuple: (Array[Array[Int]], Double)) = new FPGrowthMagnet {

    def apply(): Result = {
      val (itemsets, minSupportPercentage) = (tuple._1, tuple._2)
      val minSupport = (minSupportPercentage * itemsets.length).ceil.toInt
      new FPGrowth(minSupport, FPTree(itemsets, minSupport))
    }
  }
}
