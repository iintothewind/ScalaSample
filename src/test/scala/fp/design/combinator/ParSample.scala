package fp.design.combinator

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

import org.junit.Test

import scala.annotation.tailrec


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

  def async[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def run[A](par: Par[A])(es: ExecutorService): Future[A] = par(es)

  def get[A](par: Par[A])(es: ExecutorService): A = par(es).get

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = pa(es)
    val bf = pb(es)
    UnitFuture(f(af.get, bf.get))
  }

  def map[A, B](p: Par[A])(f: A => B): Par[B] = map2(p, unit(()))((a, _) => f(a))

  def sortPar(lp: Par[List[Int]]): Par[List[Int]] = map(lp)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    @tailrec
    def loop(plst: Par[List[A]], pars: List[Par[A]]): Par[List[A]] = pars match {
      case Nil => plst
      case x :: rs => loop(map2(x, fork(plst))(_ :: _), rs)
    }
    loop(unit(List.empty[A]), ps)
  }

  def parMap[A, B](lp: List[A])(f: A => B): Par[List[B]] = fork(sequence(lp.map(async(f))))

  def parFilter[A](xs: List[A])(f: A => Boolean): Par[List[A]] = parMap(xs.filter(f))(identity)
}

class ParSample {
  @Test
  def testMap(): Unit = {
    val x = 9
    Par.map(Par.unit(x))(identity) == Par.unit(identity(x))
  }

}