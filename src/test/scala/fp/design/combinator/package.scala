package fp.design

import java.util.concurrent.{Executors, ExecutorService}

import scala.language.implicitConversions

package object combinator {
  type Async[A] = ExecutorService => Future[A]

  implicit def asyncToPar[A](a: Async[A]): Par[A] = Par(a)

  implicit lazy val executorService: ExecutorService = Executors.newWorkStealingPool
}
