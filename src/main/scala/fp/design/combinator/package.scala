package fp.design

import java.util.concurrent.{ExecutorService, Executors}

import scala.language.implicitConversions

package object combinator {
  type Async[A] = ExecutorService => Futre[A]

  implicit def asyncToPar[A](a: Async[A]): Par[A] = Par(a)

  implicit lazy val executorService: ExecutorService = Executors.newWorkStealingPool

  sealed trait Result

  sealed case class Pass() extends Result

  sealed case class Fail[A](a: A) extends Result

  sealed case class Exception[A](a: A, e: Throwable) extends Result

}
