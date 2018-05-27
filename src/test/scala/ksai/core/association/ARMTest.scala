package ksai.core.association

import akka.actor.ActorSystem
import akka.util.Timeout
import ksai.training.validation.ValidationImplicits
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

class ARMTest extends AsyncWordSpec with Matchers with ValidationImplicits {

  implicit val actorSystem: ActorSystem = ActorSystem("ARM-Test")
  implicit val timeout: Timeout = Timeout(20 seconds)

  val itemSets = Array(
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
      val arm = ARM(itemSets, 3)

      val eventualRules = arm.learn(0.5)

      eventualRules.map(rules => assert(rules.length == 9))
    }

    "learn from pima file" in {
      val data = Source.fromFile(getClass.getResource("/pima.D38.N768.C2").getPath).getLines().map(_.split(" ").map(_.toInt)).toArray

      val arm = ARM(data, 20)

      val eventualResults = arm.learn(0.9)

      eventualResults.map(results => assert(results.size == 6803))
    }

    "learn from kosarak file" in {
      val data: Array[Array[Int]] = Source.fromFile(getClass.getResource("/kosarak.dat").getPath).getLines().map(_.split(" ").map(_.toInt)).toArray

      val arm = ARM(data, 0.003)

      val eventualResults = arm.learn(0.5)

      eventualResults.map(results => assert(results.size == 17932))
    }
  }
}
