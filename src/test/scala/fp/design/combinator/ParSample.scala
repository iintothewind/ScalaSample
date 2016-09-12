package fp.design.combinator

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, ExecutorService, Executors, TimeUnit}

import org.junit.Test

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scalaz.concurrent.Actor

sealed trait Future[A] {
  private[combinator] def apply(p: PartialFunction[Try[A], Unit]): Unit
}

sealed case class Par[A](task: Async[A]) {
  def mapWith[B, C](that: Par[B])(f: (A, B) => Try[C]): Par[C] = Par(Par.map2(this.task, that.task)(f))

  def map[B](f: A => Try[B]): Par[B] = Par(Par.map(task)(f))

  def chooser[B](choose: A => Async[B]): Par[B] = Par(Par.chooser(task)(choose))

  def runWith(es: ExecutorService, timeout: Duration = 9.seconds): Try[A] = Par.run(es, timeout)(task)
}

object Par {
  def apply[A](a: => Try[A]): Par[A] = Par(lazyUnit(a))

  def unit[A](t: Try[A]): Async[A] = es => new Future[A] {
    override private[combinator] def apply(pf: PartialFunction[Try[A], Unit]): Unit = pf(t)
  }

  def eval(es: ExecutorService)(callback: => Unit): Unit = es.submit(new Runnable {
    override def run(): Unit = callback
  })

  def fork[A](a: => Async[A]): Async[A] = es => new Future[A] {
    override private[combinator] def apply(p: PartialFunction[Try[A], Unit]): Unit = eval(es)(a(es)(p))
  }

  def lazyUnit[A](t: => Try[A]): Async[A] = fork(unit(t))

  def async[A, B](f: A => Try[B]): A => Async[B] = a => lazyUnit(f(a))

  def run[A](es: ExecutorService, timeout: Duration = 9.seconds)(a: Async[A]): Try[A] = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    a(es) {
      case Success(v) => ref.set(v); latch.countDown()
      case _ => latch.countDown()
    }
    Try(latch.await(timeout.toMillis, TimeUnit.MILLISECONDS)).flatMap {
      case true if Option(ref.get).nonEmpty => Success(ref.get)
      case _ => Failure(new IllegalStateException("Par execution error, should use Try with recover() for error handling"))
    }
  }

  def map2[A, B, C](la: Async[A], lb: Async[B])(f: (A, B) => Try[C]): Async[C] = es => new Future[C] {
    override private[combinator] def apply(p: PartialFunction[Try[C], Unit]): Unit = {
      var oa: Option[A] = None
      var ob: Option[B] = None
      val combiner = Actor[Either[Try[A],Try[B]]]{
        case Left(Success(a)) => ob match {
          case None => oa = Some(a)
          case Some(b) => eval(es)(p(f(a,b)))
        }
        case Right(Success(b))=> oa match {
          case None => ob = Some(b)
          case Some(a)=> eval(es)(p(f(a,b)))
        }
        case Left(Failure(e)) => eval(es)(p(Failure(e)))
        case Right(Failure(e)) => eval(es)(p(Failure(e)))
      }
      // If any of pa or pb is going wrong, Failure(e) will be returned, hence eval(es)(p(Failure(e))) will never be called
      combiner ! Left(run(es)(la))
      combiner ! Right(run(es)(lb))
    }
  }

  def map[A, B](async: Async[A])(f: A => Try[B]): Async[B] = map2(async, unit(Try()))((a, _) => f(a))

  def sortPar(al: Async[List[Int]]): Async[List[Int]] = map(al)(xs => Try(xs.sorted))

  def sequence[A](la: List[Async[A]]): Async[List[A]] = la.foldRight[Async[List[A]]](unit(Try(List.empty[A])))((h,t)=>map2(h,t)((x,xs)=>Try(x::xs)))

  def parMap[A, B](xs: List[A])(f: A => Try[B]): Async[List[B]] = fork(sequence(xs.map(async(f))))

  def parFilter[A](xs: List[A])(f: A => Try[Boolean]): Async[List[A]] = parMap(xs.filter(f.andThen(_.getOrElse(true))))(a => Try(identity(a)))

  def chooser[A, B](a: Async[A])(choose: A => Async[B]): Async[B] = es => choose(run(es)(a).get)(es)

  def choice[A](cond: Async[Boolean])(t: Async[A], f: Async[A]): Async[A] = chooser(cond) { case true => t; case false => f }
}

class ParSample {
  @Test
  def testAsync(): Unit = {
    val ps = List("a", "b", "c").map(Par.async(i => Try(i.toInt).recover { case _ => 0 }))
    ps.map(_.runWith(Executors.newWorkStealingPool())).foreach(_.ensuring(_.isSuccess))
  }

  @Test
  def testAsyncWithoutRecovery(): Unit = {
    Par.lazyUnit(Try("a".toInt)).runWith(Executors.newWorkStealingPool()).ensuring(_.isFailure)
  }

  @Test
  def testMap2(): Unit = {
    val mp2 = Par.map2(Par.lazyUnit(Try("a".toInt).recover { case _ => 0 }), Par.lazyUnit(Try("b".toInt).recover { case _ => 0 }))((l, r) => Try(l + r))
    mp2.runWith(Executors.newWorkStealingPool()).foreach(_.ensuring(_ == 0))
  }

  @Test
  def testMap(): Unit = {
    Par.map(Par.lazyUnit(Try("a".toInt).recover { case _ => 0 }))(i => Try(i + 1))
      .runWith(Executors.newWorkStealingPool()).foreach(_.ensuring(_ == 1))
  }

  @Test
  def testSortPar(): Unit = {
    val lst = List(9, -2, 3, -6, 5, 2, 1)
    Par.sortPar(Par.lazyUnit(Try(lst)))
      .runWith(Executors.newWorkStealingPool())
      .foreach(_.ensuring(_ == lst.sorted))
  }

  @Test
  def testParMap(): Unit = {
    Par.parMap(List.range(1, 100))(i => Try(i * 2))
      .runWith(Executors.newWorkStealingPool())
      .foreach(_.ensuring(_ == List.range(1, 100).map(_ * 2)))
  }

  @Test
  def testParMapWithoutRecovery(): Unit = {
    Par.parMap(List("b", "a", "2", "3", "4", "5"))(i => Try(i.toInt))
      .runWith(Executors.newWorkStealingPool())
      .isFailure
  }

  @Test
  def testChooser(): Unit = {
    Par.unit(Try(List(9, -2, 3, -6, 5, 2, 1))).chooser(xs => Par.lazyUnit(Try(xs.max)))
      .runWith(Executors.newWorkStealingPool())
      .foreach(_.ensuring(_ == 9))
  }
}