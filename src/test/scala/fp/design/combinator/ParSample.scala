package fp.design.combinator

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}


//sealed trait Par[+A] {
//  def get: A
//
//  def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C]
//}

private case class UnitFuture[A](get: A) extends Future[A] {
  override def isCancelled: Boolean = false

  override def get(timeout: Long, unit: TimeUnit): A = get

  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

  override def isDone: Boolean = true
}

object Par {


  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](par: Par[A])(es: ExecutorService): Future[A] = par(es)

  def get[A](par: Par[A])(es: ExecutorService): A = par(es).get

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = pa(es)
    val bf = pb(es)
    UnitFuture(f(af.get, bf.get))
  }

  def async[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
}

class ParSample {


}
