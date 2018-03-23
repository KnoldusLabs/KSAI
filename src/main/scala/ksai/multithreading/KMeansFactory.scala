package ksai.multithreading

import akka.actor._
import akka.routing.RoundRobinPool
import akka.pattern.pipe
import ksai.core.cluster.{BBDKDTree, KMeans}
import ksai.multithreading.KAsyncExec._
import ksai.logging.ScalaLogging.logger._

import scala.concurrent.Future

object KMeansFactory {

  val system = ActorSystem()

  def getKMeansGeneratorActor() = {
    system.actorOf(RoundRobinPool(Runtime.getRuntime.availableProcessors()).props(Props[KMeansGeneratorActor]))
  }
}

case class GenerateKMeansWithRuns(
                                   data: List[List[Double]],
                                   k: Int,
                                   maxIter: Int,
                                   runs: Int,
                                   bbdTree: Option[BBDKDTree]
                                 )


class KMeansGeneratorActor extends Actor{
  override def receive: Receive = {
    case GenerateKMeansWithRuns(data: List[List[Double]], k: Int, maxIter: Int, runs: Int, bbdTree) =>
      implicit val sys = context.system
      info("Inside actor for kmeans")
      val actorSender = sender()
      val kmeans: Future[KMeans] = KMeans(data, k, maxIter, runs, bbdTree)
      kmeans pipeTo actorSender
  }
}
