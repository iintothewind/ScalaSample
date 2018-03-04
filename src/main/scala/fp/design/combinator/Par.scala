package fp.design.combinator

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ExecutorService, TimeUnit, _}

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scalaz.concurrent.Actor

sealed case class Par[A](task: Async[A]) {
  def mapWith[B, C](that: Par[B])(f: (A, B) => Try[C]): Par[C] = Par(Par.map2(this.task, that.task)(f))

  def map[B](f: A => Try[B]): Par[B] = Par(Par.map(task)(f))

  def chooser[B](choose: A => Async[B]): Par[B] = Par(Par.chooser(task)(choose))

  def runWith(es: ExecutorService, timeout: Duration = 9.seconds): Try[A] = Par.run(es, timeout)(task)

  def run(timeout: Duration = 9.seconds)(implicit executor: ExecutorService): Try[A] = Par.run(executor, timeout)(task)
}

sealed trait Futre[A] {
  private[combinator] def apply(p: PartialFunction[Try[A], Unit]): Unit
}

object Par {
  def apply[A](a: => Try[A]): Par[A] = Par(lazyUnit(a))

  def fork[A](a: => Async[A]): Async[A] = es => new Futre[A] {
    override private[combinator] def apply(p: PartialFunction[Try[A], Unit]): Unit = eval(es)(a(es)(p))
  }

  def unit[A](t: Try[A]): Async[A] = es => new Futre[A] {
    override private[combinator] def apply(pf: PartialFunction[Try[A], Unit]): Unit = pf(t)
  }

  def lazyUnit[A](t: => Try[A]): Async[A] = fork(unit(t))

  def eval(es: ExecutorService)(callback: => Unit): Unit = es.submit(new Runnable {
    override def run(): Unit = callback
  })


  def async[A, B](f: A => Try[B]): A => Async[B] = a => lazyUnit(f(a))

  def run[A](es: ExecutorService, timeout: Duration = 9.seconds)(a: Async[A]): Try[A] = {
    val ref = new AtomicReference[Try[A]]
    val latch = new CountDownLatch(1)
    a(es) {
      case s@Success(_) => ref.set(s); latch.countDown()
      case f@Failure(_) => ref.set(f); latch.countDown()
    }
    Try(latch.await(timeout.toMillis, TimeUnit.MILLISECONDS)).flatMap {
      case true => ref.get
      case _ => Failure(new TimeoutException("Par execution timeout."))
    }
  }

  def map2[A, B, C](la: Async[A], lb: Async[B])(f: (A, B) => Try[C]): Async[C] = es => new Futre[C] {
    override private[combinator] def apply(p: PartialFunction[Try[C], Unit]): Unit = {
      var oa: Option[A] = None
      var ob: Option[B] = None
      val combiner = Actor[Either[Try[A], Try[B]]] {
        case Left(Success(a)) => ob match {
          case None => oa = Some(a)
          case Some(b) => eval(es)(p(f(a, b)))
        }
        case Right(Success(b)) => oa match {
          case None => ob = Some(b)
          case Some(a) => eval(es)(p(f(a, b)))
        }
        case Left(Failure(e)) => eval(es)(p(Failure(e)))
        case Right(Failure(e)) => eval(es)(p(Failure(e)))
      }
      combiner ! Left(run(es)(la))
      combiner ! Right(run(es)(lb))
    }
  }

  def map[A, B](async: Async[A])(f: A => Try[B]): Async[B] = map2(async, unit(Try(Unit)))((a, _) => f(a))

  def sortPar(al: Async[List[Int]]): Async[List[Int]] = map(al)(xs => Try(xs.sorted))

  def sequence[A](la: List[Async[A]]): Async[List[A]] = la.foldRight[Async[List[A]]](unit(Try(List.empty[A])))((h, t) => map2(h, t)((x, xs) => Try(x :: xs)))

  def parMap[A, B](xs: List[A])(f: A => Try[B]): Async[List[B]] = fork(sequence(xs.map(async(f))))

  def parFilter[A](xs: List[A])(f: A => Try[Boolean]): Async[List[A]] = parMap(xs.filter(f.andThen(_.getOrElse(true))))(a => Try(identity(a)))

  def chooser[A, B](a: Async[A])(choose: A => Async[B]): Async[B] = es => choose(run(es)(a).get)(es)

  def choice[A](cond: Async[Boolean])(t: Async[A], f: Async[A]): Async[A] = chooser(cond) { case true => t; case false => f }
}
