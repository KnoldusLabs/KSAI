package ksai.kernels


trait Kernel {

  def k(x: Double, y: Double): Double

}
