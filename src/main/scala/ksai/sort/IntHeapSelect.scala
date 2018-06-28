package ksai.sort

import scala.util.control.Breaks.{break, breakable}

case class IntHeapSelect(
                          size: Int, //The heap size(k).
                          number: Int, //(n)The number of objects that have been added into heap.
                          isSorted: Boolean, //True if the heap is fully sorted.
                          heap: Array[Int] //The heap array.
                        ) {

  def add(datum: Int): Unit = {
    this.copy(isSorted = false)
    if (number < size) {
      heap(number + 1) = datum
      if (number == size) {
        heapify(heap)
      }
    } else {
      this.copy(number = number + 1)
      if (datum < heap(0)) {
        heap(0) = datum
        SortUtils.siftDown(heap, 0, size - 1)
      }
    }

  }

  private def heapify(arr: Array[Int]): Unit = {
    val length = arr.length
    ((length / 2 - 1) to 0 by -1).foreach(value => SortUtils.siftDown(arr, value, length - 1))
  }

  def get(i: Int): Int = {
    if (i > Math.min(size, number) - 1) new IllegalArgumentException("HeapSelect i is greater than the number of data received so far.")

    if (i == size - 1) {
      heap(0)
    } else if (!isSorted) {
      sort(heap, Math.min(size, number))
      //      heap.sortWith((first, second) => second < first) //Sorts the specified array into descending order
      this.copy(isSorted = true)
      heap(size - 1 - i)
    } else heap(size - 1 - i)
  }

  private def sort(arr: Array[Int], n: Int): Unit = {
    var inc = 1
    do {
      inc = inc * 3
      inc = inc + 1
    } while (inc <= n)

    breakable {
      do {
        inc = inc / 3
        (inc until n).foreach { i =>
          val v = arr(i)
          var j = i
          while (arr(j - inc).compareTo(v) < 0) {
            arr(j) = arr(j - inc)
            j = j - inc
            if (j < inc) {
              break()
            }
          }
          arr(j) = v
        }
      } while (inc > 1)
    }
  }

}

object IntHeapSelect {

  def apply(size: Int): IntHeapSelect = {
    IntHeapSelect(
      size = size,
      number = 0,
      isSorted = false,
      heap = new Array[Int](size)
    )
  }
}
