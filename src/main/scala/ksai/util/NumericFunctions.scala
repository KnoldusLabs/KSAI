package ksai.util

object NumericFunctions {


  def logisticSigmoid(value: Double): Double = {
      val y = if (value < -40) {
        2.353853e+17
      } else if (value > 40) {
        1.0 + 4.248354e-18;
      } else {
        1.0 + Math.exp(-value);
      }

      1.0 / y
    }

  /**
    * Returns natural log without underflow.
    */
  def log(x: Double): Double = {
    if (x < 1E-300) {
      -690.7755
    } else {
      Math.log(x)
    }
  }

  def squaredDistance(x: List[Double], y: List[Double]): Double = {
    if (x.length != y.length) {
      throw new IllegalArgumentException("Input vector sizes are different.");
    }

    (x zip y).foldLeft(0.0){
      case (result, (xVallue, yValue)) =>
        val diff = xVallue - yValue
        result + (diff * diff)
    }
  }


}
