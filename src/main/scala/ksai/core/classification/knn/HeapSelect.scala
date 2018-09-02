package ksai.core.classification.knn

import scala.util.Try
import scala.util.control.Breaks._

case class HeapSelect(
                       k: Int,
                       n: Int,
                       heap: Array[Neighbor] = Array.empty
                     ) {
  var sorted: Boolean = false

  def heapify(arr: Array[Neighbor]): Unit = {
    val n = arr.length
    (0 until n / 2).reverse.map { i =>
      shiftDown(arr, i, n - 1)
    }
  }

  def peak(): Neighbor = heap(0)

  def updatePeek(neighbour: Neighbor) = {
    heap.update(0, neighbour)
  }

  /**
    * In case of avoiding creating new objects frequently, one may check and
    * update the peek object directly and call this method to sort the internal
    * array in heap order.
    */
  def heapify(): Unit = {
    if(n < k) {
      throw new IllegalStateException()
    } else {
      shiftDown(heap, 0, k - 1)
    }
  }

  def sort(): Array[Neighbor] ={
    if(!sorted){
      sort(heap, Math.min(k,n))
      sorted = true
      heap
    }else{
      heap
    }
  }

  def shiftDown(arr: Array[Neighbor], k: Int, n: Int): Unit = {
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
        ShortUtil.swap(arr, ktemp, j)
        ktemp = j
      }
    }
  }

  /**
    * Sorts the specified array into descending order. It is based on Shell
    * sort, which is very efficient because the array is almost sorted by
    * heapifying.
    */
  def sort(a: Array[Neighbor], n: Int) ={
    var inc = 1
    do{
      inc = inc * 3
      inc = inc + 1
    }while(inc <= n)

    breakable {
      do {
        inc = inc / 3
        (inc until n).map { i =>
          val v = a(i)
          var j = i
          while (a(j - inc).compareTo(v) < 0) {
            a(j) = a(j - inc)
            j = j - inc
            if (j < inc) {
              break()
            }
          }
          a(j) = v
        }
      } while (inc > 1)
    }
  }

}

object ShortUtil {



  /**
    * Swap two elements of array
    */
  def swap(arr: Array[Neighbor], i: Int, j: Int) = {
    val a = arr(i)
    arr(i) = arr(j)
    arr(j) = a
  }

}

object HeapSelect {

  def apply(_k: Int, neighbor: Neighbor): HeapSelect = {
    HeapSelect(k = _k, n = _k, heap = Array.fill(_k)(neighbor))
  }


}
