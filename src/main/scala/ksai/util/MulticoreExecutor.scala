package ksai.util

import java.util.concurrent.Executors

import scala.util.Try

object MulticoreExecutor {

  object IO {
    val CONCURRENCY_FACTOR = 1
    private val numThreads: Int = Try {
      val env = System.getProperty("KSAI.threads")
      env.toInt
    } match {
      case _ => Runtime.getRuntime.availableProcessors()
    }

    implicit lazy val customExecutionContext: concurrent.ExecutionContext = concurrent.ExecutionContext.fromExecutor(
      Executors.newFixedThreadPool(numThreads * CONCURRENCY_FACTOR)
    )

    def getThreadPoolSize: Int = numThreads
  }

}
