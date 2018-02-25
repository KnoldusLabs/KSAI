package ksai.training.validation

import ksai.util.NumericFunctions

object AdjustRandIndex {

  private def getCount(y1: List[Int], y2: List[Int], label1: List[Int], label2: List[Int]) = {
    (0 to label1.size - 1).toList.map {
      case i =>
        (0 to label2.size - 1).toList.map {
          case j =>
            (y1.zip(y2)).foldLeft(0) {
              case (matchCount, (y1Value, y2Value)) =>
                if (y1Value == label1(i) && y2Value == label2(j)) {
                  matchCount + 1
                } else matchCount
            }
        }
    }
  }

  private def getCount1(count: List[List[Int]]) = {
    count.map(_.sum)
  }

  private def getCount2(count: List[List[Int]]) = {
    count.tail.foldLeft(count.head) {
      case (count2Result, countValues) =>
        (count2Result zip countValues).map { case (c2r, cv) => c2r + cv }
    }
  }

  private def getRand1(count: List[List[Int]], randFunc: (Double, Int) => Double) = {
    count.foldLeft(0.0) {
      case (result, crow) =>
        result + crow.foldLeft(result) {
          case (rowResult, cell) => randFunc(rowResult, cell)
        }
    }
  }

  private def getRand2a(count1: List[Int], randFunc: (Double, Int) => Double) = {
    count1.foldLeft(0.0) {
      case (result, cell) =>
      randFunc(result, cell)
    }
  }

  private def getRand2b(count2: List[Int], randFunc: (Double, Int) => Double) = {
    count2.foldLeft(0.0) {
      case (result, cell) => randFunc(result, cell)
    }
  }

  def measure(y1: List[Int], y2: List[Int]): Double = {
    if (y1.length != y2.length) {
      throw new IllegalArgumentException(s"The vector sizes don't match: ${y1.length} != ${y2.length}")
    }

    val label1: List[Int] = y1.distinct
    val label2: List[Int] = y2.distinct
    val count: List[List[Int]] = getCount(y1, y2, label1, label2)

    val count1 = getCount1(count)
    val count2 = getCount2(count)

    val rand1 = getRand1(count, {case (rowResult, cell) => if (cell >= 2) {
      rowResult + NumericFunctions.choose(cell, 2)
    } else rowResult})

    val rand2a = getRand2a(count1, {
      case (result, cell) => if (cell >= 2) {
        result + NumericFunctions.choose(cell, 2)
      } else result
    })

    val rand2b = getRand2b(count2, {
      case (result, cell) =>
      if (cell >= 2) {
        result + NumericFunctions.choose(cell, 2)
      } else result
    })

    val rand3 = (rand2a * rand2b) / NumericFunctions.choose(y1.size, 2)
    val rand_N = rand1 - rand3
    val rand4 = (rand2a + rand2b) / 2
    val rand_D = rand4 - rand3
    val rand = rand_N / rand_D

    rand
  }

  def measureRand(y1: List[Int], y2: List[Int]): Double = {
    if (y1.length != y2.length) {
      throw new IllegalArgumentException(String.format(s"The vector sizes don't match: ${y1.length} != ${y2.length}"))
    }

    val label1: List[Int] = y1.distinct
    val label2: List[Int] = y2.distinct
    val count: List[List[Int]] = getCount(y1, y2, label1, label2)

    val count1 = getCount1(count)
    val count2 = getCount2(count)

    val randT = getRand1(count, {case (rowResult, cell) => rowResult + (cell * cell)}) - y1.size

    val randP = getRand2a(count1, {
      case (result, cell) => result + (cell * cell) - y1.size
    })

    val randQ = getRand2b(count2, {
      case (result, cell) => result + (cell * cell) - y1.size
    })

    val rand = (randT - 0.5 * randP - 0.5 * randQ + NumericFunctions.choose(y1.size, 2)) / NumericFunctions.choose(y1.size, 2)

    rand
  }

}
