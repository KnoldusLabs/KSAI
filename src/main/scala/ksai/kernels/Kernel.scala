package ksai.kernels


trait Kernel[A] {

  def k(x: A, y: A): Double

}
