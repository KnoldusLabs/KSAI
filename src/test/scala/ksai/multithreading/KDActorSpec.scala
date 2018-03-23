package ksai.multithreading

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActors, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

/**
  * Created by pranjut on 24/2/18.
  */
class KDActorSpec extends TestKit(ActorSystem("KDActor")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "The KDActor" must {

    "send back result as false" in {
      val echo = system.actorOf(Props[KMeansActor])
      echo ! PruneDetail(List(1.0, 2.0, 3.0), List(.1, .2, .3),
        List(List(1.0, 2.0, 3.0), List(4.0, 5.0, 6.0), List(7.0, 8.0, 9.0)),
        1, 1)
      expectMsg(false)
    }

    "send back result as true" in {
      val echo = system.actorOf(Props[KMeansActor])
      echo ! PruneDetail(List(1.0, 2.0, 3.0), List(.1, .2, .3),
        List(List(1.0, 2.0, 3.0), List(4.0, 5.0, 6.0), List(7.0, 8.0, 9.0)),
        1, 2)
      expectMsg(false)
    }

  }
}
