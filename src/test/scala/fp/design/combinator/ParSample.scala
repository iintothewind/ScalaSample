package fp.design.combinator

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, ExecutorService, Executors, TimeUnit}

import org.junit.Test

import scala.concurrent.duration._
import scala.util.{Success, Try}
import scalaz.concurrent.Actor

sealed trait Future[A] {
  private[combinator] def apply(p: PartialFunction[Try[A], Unit]): Unit
}

object Par {
  def unit[A](a: Try[A]): Par[A] = es => new Future[A] {
    override private[combinator] def apply(p: PartialFunction[Try[A], Unit]): Unit = p(a)
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Runnable {
    override def run(): Unit = r
  })

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    override private[combinator] def apply(p: PartialFunction[Try[A], Unit]): Unit = eval(es)(a(es)(p))
  }

  def lazyUnit[A](t: => Try[A]): Par[A] = fork(unit(t))

  def async[A, B](f: A => Try[B]): A => Par[B] = a => lazyUnit(f(a))

  def run[A](es: ExecutorService, timeout: Duration = 9.seconds)(p: Par[A]): Option[A] = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) {
      case Success(a) => ref.set(a); latch.countDown()
      case _ => latch.countDown()
    }
    Try(latch.await(timeout.toMillis, TimeUnit.MILLISECONDS)).filter(_ == false).foreach { x =>
      println(x)
      throw new IllegalStateException("Par execution failed, should use Try with recover() to error handling.")
    }
    Option(ref.get)
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => Try[C]): Par[C] = es => new Future[C] {
    override private[combinator] def apply(p: PartialFunction[Try[C], Unit]): Unit = {
      var oa: Option[A] = None
      var ob: Option[B] = None
      val combiner = Actor[Either[A,B]]{
        case Left(a) => ob match {
          case None => oa = Some(a)
          case Some(b) => eval(es)(p(f(a,b)))
        }
        case Right(b)=> oa match {
          case None => ob = Some(b)
          case Some(a)=> eval(es)(p(f(a,b)))
        }
      }
      // If any of pa or pb is going wrong, message will never be sent to combiner
      // hence f(a,b) will never be called, and Par[C] will never be executed
      run(es)(pa).foreach(combiner ! Left(_))
      run(es)(pb).foreach(combiner ! Right(_))
    }
  }

  def map[A, B](p: Par[A])(f: A => Try[B]): Par[B] = map2(p, unit(Try()))((a, _) => f(a))

  def sortPar(lp: Par[List[Int]]): Par[List[Int]] = map(lp)(xs => Try(xs.sorted))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(Try(List.empty[A])))((h,t)=>map2(h,t)((x,xs)=>Try(x::xs)))

  def parMap[A, B](lp: List[A])(f: A => Try[B]): Par[List[B]] = fork(sequence(lp.map(async(f))))

  def parFilter[A](xs: List[A])(f: A => Try[Boolean]): Par[List[A]] = parMap(xs.filter(f.andThen(_.getOrElse(true))))(a => Try(identity(a)))
}

class ParSample {
  @Test
  def testAsync(): Unit = {
    val ps = List("a", "b", "c").map(Par.async(i => Try(i.toInt).recover { case _ => 0 }))
    println(ps.map(Par.run(Executors.newWorkStealingPool())(_)))
  }

  @Test
  def testAsyncWithoutRecovery(): Unit = {
    val ps = List("a", "b", "c").map(Par.async(i => Try(i.toInt)))
    println(ps.map(Par.run(Executors.newWorkStealingPool())(_)))
  }

  @Test
  def testMap2(): Unit = {
    val mp2 = Par.map2(Par.lazyUnit(Try("a".toInt).recover { case _ => 0 }), Par.lazyUnit(Try("b".toInt).recover { case _ => 0 }))((l, r) => Try(l + r))
    println(Par.run(Executors.newWorkStealingPool())(mp2))
  }

  @Test
  def testMap(): Unit = {
    val mp = Par.map(Par.lazyUnit(Try("a".toInt).recover { case _ => 0 }))(i => Try(i + 1))
    println(Par.run(Executors.newWorkStealingPool())(mp))
  }

  @Test
  def testSortPar(): Unit = {
    val pl = Par.sortPar(Par.lazyUnit(Try(List(9, -2, 3, -6, 5, 2, 1))))
    println(Par.run(Executors.newWorkStealingPool())(pl))
  }

  @Test
  def testParMap(): Unit = {
    val p = Par.parMap(List.range(1, 100))(i => Try(i * 2))
    val x = Par.run(Executors.newWorkStealingPool())(p)
    println(x)
  }

  @Test
  def testExceptionHandling(): Unit = {
    val p = Par.parMap(List("1", "a", "2", "3", "4", "5"))(i => Try(i.toInt).recover { case _ => 0 })
    val x = Par.run(Executors.newWorkStealingPool())(p)
    println(x)
  }
}