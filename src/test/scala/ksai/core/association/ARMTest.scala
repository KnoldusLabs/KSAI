package ksai.core.association

import ksai.training.validation.ValidationImplicits
import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class ARMTest extends WordSpec with Matchers with ValidationImplicits {

  val itemsets = Array(
    Array(1, 3),
    Array(2),
    Array(4),
    Array(2, 3, 4),
    Array(2, 3),
    Array(2, 3),
    Array(1, 2, 3, 4),
    Array(1, 3),
    Array(1, 2, 3),
    Array(1, 2, 3))

  "ARM" should {

    "learn from small dataset" in {
      val arm = ARM(itemsets, 3)

      val rules = arm.learn(0.5)

      assert(rules.length == 9)
    }

    "learn from pima file" in {
      val data = Source.fromFile("/home/knoldus/smile/shell/src/universal/data/transaction/pima.D38.N768.C2").getLines().map(_.split(" ").map(_.toInt)).toArray

      val arm = ARM(data, 20)

      val results = arm.learn(0.9)

      results.foreach(println)

      assert(results.size == 6803)
    }

    "learn from kosarak file" in {
      val data: Array[Array[Int]] = Source.fromFile("/home/knoldus/smile/shell/src/universal/data/transaction/kosarak.dat").getLines().map(_.split(" ").map(_.toInt)).toArray

      val arm = ARM(data, 0.003)

      val results = arm.learn(0.5)

      assert(results.size == 17932)
    }
  }
}
