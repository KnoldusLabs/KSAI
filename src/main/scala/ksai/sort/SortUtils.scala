package ksai.sort

import scala.util.control.Breaks.{break, breakable}

object SortUtils {

  def siftDown(arr: Array[Int], k: Int, n: Int): Unit = {
    var ktemp = k
    breakable {
      while(2 * ktemp <= n){
        var j = 2 * ktemp
        if (j < n && arr(j).compareTo(arr(j + 1)) < 0) {
          j = j + 1
        }
        val comp: Int = arr(ktemp).compareTo(arr(j))
        if (comp >= 0) {
          break()
        }
        swap(arr, ktemp, j)
        ktemp = j
      }
    }
  }


  def swap(arr: Array[Int], first: Int, second: Int):Unit = {
    val temp = arr(first)
    arr(first) = arr(second)
    arr(second) = temp
  }

}
