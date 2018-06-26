package ksai.kernels


class LinearKernel extends Kernel[Array[Double]]{
  override def k(x: Array[Double], y: Array[Double]): Double = {
    val (result, _) = x.foldLeft((0.0, 0)){
      case ((result, index), elem) => (result + (elem * y(index)), index + 1)
    }
    result
  }

}
