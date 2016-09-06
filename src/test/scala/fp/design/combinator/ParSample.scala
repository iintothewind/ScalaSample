package fp.design.combinator

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Executors, Callable, ExecutorService}

import com.twitter.util.CountDownLatch
import org.junit.Test
import scala.annotation.tailrec
import scala.util.Try
import scalaz.concurrent.Actor

sealed trait Future[A] {
  private[combinator] def apply(k: A => Unit): Unit
}

object Par {
  def unit[A](a: A): Par[A] = es => new Future[A] {
    override private[combinator] def apply(k: A => Unit): Unit = k(a)
  }

  def delay[A](a: => A): Par[A] = es => new Future[A] {
    override private[combinator] def apply(k: A => Unit): Unit = k(a)
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
    override def call = r
  })

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    override private[combinator] def apply(k: A => Unit): Unit = eval(es)(a(es)(k))
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def async[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => Try(ref.set(a)).failed.recover { case _ => latch.countDown() } }
    latch.await()
    ref.get
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    override private[combinator] def apply(k: C => Unit): Unit = {
      var oa: Option[A] = None
      var ob: Option[B] = None
      val combiner = Actor[Either[A,B]]{
        case Left(a) => ob match {
          case None => oa = Some(a)
          case Some(b) => eval(es)(k(f(a,b)))
        }
        case Right(b)=> oa match {
          case None => ob = Some(b)
          case Some(a)=> eval(es)(k(f(a,b)))
        }
      }
      pa(es)(a => combiner ! Left(a))
      pb(es)(b => combiner ! Right(b))
    }
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
  def testParMap(): Unit = {
    val p = Par.parMap(List.range(1, 100))(_ * 2)
    val x = Par.run(Executors.newWorkStealingPool())(p)
    println(x)
  }

}