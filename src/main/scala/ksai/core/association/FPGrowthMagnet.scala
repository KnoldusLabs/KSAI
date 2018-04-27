package ksai.core.association

trait FPGrowthMagnet {

  type Result = FPGrowth

  def apply(): Result
}

object FPGrowthMagnet {

  implicit def forFrequency(tuple: (Array[Int], Int)) = new FPGrowthMagnet {

    def apply(): Result = {
      val (frequency, minSupport) = (tuple._1, tuple._2)
      new FPGrowth(minSupport, FPTree(frequency, minSupport))
    }
  }

  implicit def forIntegerMinSupport(tuple: (Array[Array[Int]], Int)) = new FPGrowthMagnet {

    def apply(): Result = {
      val (itemsets, minSupport) = (tuple._1, tuple._2)
      new FPGrowth(minSupport, FPTree(itemsets, minSupport))
    }
  }

  implicit def forPercentageMinSupport(tuple: (Array[Array[Int]], Double)) = new FPGrowthMagnet {

    def apply(): Result = {
      val (itemsets, minSupportPercentage) = (tuple._1, tuple._2)
      val minSupport = minSupportPercentage.ceil.toInt
      new FPGrowth(minSupport, FPTree(itemsets, minSupport))
    }
  }
}
