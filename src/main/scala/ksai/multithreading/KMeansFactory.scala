package ksai.multithreading

import akka.actor.{Actor, ActorSystem, Props}
import akka.routing.RoundRobinPool
import akka.pattern._
import ksai.core.cluster.KMeans

import scala.concurrent.Future

object KMeansFactory {

  val system = ActorSystem()

  def getKMeansGeneratorActor() = {
    system.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors() * 2).props(Props[KMeansGeneratorActor]))
  }
}

case class GenerateKMeansWithRuns(
                                   data: List[List[Double]],
                                   k: Int,
                                   maxIter: Int,
                                   runs: Int
                                 )


class KMeansGeneratorActor extends Actor{
  override def receive: Receive = {
    case GenerateKMeansWithRuns(data: List[List[Double]], k: Int, maxIter: Int, runs: Int) =>
      val kmeans: Future[KMeans] = KMeans(data, k, maxIter, runs)
      kmeans pipeTo sender()
  }
}
