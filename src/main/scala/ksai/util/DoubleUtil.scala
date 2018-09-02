package ksai.util

object DoubleUtil {

  implicit class RicherDouble(val d: Double) extends AnyVal {
    def nan = java.lang.Double.isNaN(d)
  }

}