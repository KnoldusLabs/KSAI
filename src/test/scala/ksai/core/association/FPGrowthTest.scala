package ksai.core.association

import ksai.core.association.fptree.FPGrowth
import ksai.training.validation.ValidationImplicits
import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class FPGrowthTest extends WordSpec with Matchers with ValidationImplicits {

  val itemsets =
    Array(Array(1, 3),
      Array(2),
      Array(4),
      Array(2, 3, 4),
      Array(2, 3),
      Array(2, 3),
      Array(1, 2, 3, 4),
      Array(1, 3),
      Array(1, 2, 3),
      Array(1, 2, 3))

  "FP-Growth" should {

    "learn with 0 arguments" in {

      val fPGrowth = FPGrowth(itemsets, 3)
      val results = fPGrowth.learn()

      assert(results.size == 8)
      /*assert(3 == results(0).support)
      assert(1 == results(0).items.length)
      assert(4 == results(0).items(0))
      assert(5 == results(1).support)
      assert(1 == results(1).items.length)
      assert(1 == results(1).items(0))

      assert(6 == results(6).support)
      assert(2 == results(6).items.length)
      assert(3 == results(6).items(0))
      assert(2 == results(6).items(1))

      assert(8 == results(7).support)
      assert(1 == results(7).items.length)
      assert(3 == results(7).items(0))*/
    }

    "learn with values in Pima file" in {
      val data = Source.fromFile("/home/knoldus/smile/shell/src/universal/data/transaction/pima.D38.N768.C2").getLines().map(_.split(" ").map(_.toInt)).toArray

      val fPGrowth = FPGrowth(data, 20)

      val results = fPGrowth.learn(Some(System.out))

      assert(results == 1803)
    }

    "learn with values in Kosarak file" in {
      val data = Source.fromFile("/home/knoldus/smile/shell/src/universal/data/transaction/kosarak.dat").getLines().map(_.split(" ").map(_.toInt)).toArray

      val fPGrowth = FPGrowth(data, 1500)

      val results = fPGrowth.learn()

      assert(results.size == 219725)
    }
  }
}
