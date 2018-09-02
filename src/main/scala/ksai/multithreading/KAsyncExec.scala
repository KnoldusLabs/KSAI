package ksai.multithreading


import java.util.concurrent.Executors

import scala.concurrent.{ ExecutionContext, ExecutionContextExecutor }

object KAsyncExec {
  implicit val kasync: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() * 4))
}
