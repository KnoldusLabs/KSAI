package ksai.core.association.totalsupporttree

import ksai.core.association.fptree.FPGrowth
import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class TotalSupportTreeTest extends WordSpec with Matchers {

  "Total Support Tree" should {
    "get frequent items from kosarak file" in {
      val data = Source.fromFile("/home/knoldus/smile/shell/src/universal/data/transaction/kosarak.dat").getLines().map(_.split(" ").map(_.toInt)).toArray

      val fPGrowth = FPGrowth(data, 1500)

      val ttree = fPGrowth.buildTotalSupportTree()

      assert(ttree.get.getFrequentItemsets.size == 219725)
    }
  }
}
