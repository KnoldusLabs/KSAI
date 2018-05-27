package ksai.core.association.totalsupporttree

import akka.actor.ActorSystem
import akka.util.Timeout
import ksai.core.association.fptree.FPGrowth
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

class TotalSupportTreeTest extends WordSpec with Matchers {

  implicit val actorSystem: ActorSystem = ActorSystem("TotalSupportTree-Test")
  implicit val timeout: Timeout = Timeout(20 seconds)

  "Total Support Tree" should {
    "get frequent items from pima file" in {
      val data = Source.fromFile(getClass.getResource("/pima.D38.N768.C2").getPath).getLines().map(_.split(" ").map(_.toInt)).toArray

      val fPGrowth = FPGrowth(data, 20)

      val eventualTtree = fPGrowth.buildTotalSupportTree()

      eventualTtree.map(ttree => assert(ttree.get.getFrequentItemsets.size == 1803))
    }

    "get frequent items from kosarak file" in {
      val data = Source.fromFile(getClass.getResource("/kosarak.dat").getPath).getLines().map(_.split(" ").map(_.toInt)).toArray

      val fPGrowth = FPGrowth(data, 1500)

      val eventualTtree = fPGrowth.buildTotalSupportTree()

      eventualTtree.map(ttree => assert(ttree.get.getFrequentItemsets.size == 219725))
    }
  }
}
